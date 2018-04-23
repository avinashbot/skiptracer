module Skiptracer.Eval (
    module Skiptracer.Eval.Data,

    eval,
    fromExp,
    isFinal
) where

import           Data.Maybe            (fromJust, fromMaybe)
import qualified Skiptracer.Eval.Bind  as Bind
import           Skiptracer.Eval.Data  (Ctx (..), State (..))
import qualified Skiptracer.Eval.Let   as Let
import qualified Skiptracer.Eval.Match as Match
import           Skiptracer.Heap       (Heap (..))
import qualified Skiptracer.Heap       as Heap
import           Skiptracer.Syntax     (Alt (..), Exp (..), Pat (..))
import qualified Skiptracer.Syntax     as Syntax

-- | Whether the state is in WHNF
isFinal :: State -> Bool
isFinal (State _ [] e) | Syntax.isValue e = True
isFinal _              = False

-- | Create a new state from a simple expression.
-- Basically the State equivalent to "pure"
fromExp :: Exp -> State
fromExp = State Heap.empty []

-- | Evaluate a primitive operation.
primOp :: String -> Exp -> Exp -> Exp
primOp "+"  (Num m) (Num n) = Num (m + n)
primOp "-"  (Num m) (Num n) = Num (m - n)
primOp "*"  (Num m) (Num n) = Num (m * n)
primOp "<"  (Num m) (Num n) = Log (m < n)
primOp ">"  (Num m) (Num n) = Log (m > n)
primOp "<=" (Num m) (Num n) = Log (m <= n)
primOp ">=" (Num m) (Num n) = Log (m >= n)
primOp "==" (Num m) (Num n) = Log (m == n)
primOp "/=" (Num m) (Num n) = Log (m /= n)
primOp "==" (Log m) (Log n) = Log (m == n)
primOp "/=" (Log m) (Log n) = Log (m /= n)
primOp s    e1      e2      = error $ "invalid primOp: " ++ unwords [show e1, s, show e2]

eval :: State -> State

--
-- PatMatCtx
--

eval (State h (PatMatCtx p : cs) ex)
    | Match.matchable ex p = State h cs ex

eval (State h (PatMatCtx (PCon n (p:ps)) : cs) ex)
    | Syntax.isValue ex =
        case ex of
            (Con m (q:qs)) -> State h (ConMatCtx n [] p (zip ps qs) : cs) q
            _              -> error "Error in pattern matching"

--
-- ConMatCtx
--

eval (State h (ConMatCtx n es p us : cs) ex)
    | Match.matchable ex p =
        case us of
            []            -> State h cs (Con n (es ++ [ex]))
            (up, ue) : ux -> State h (ConMatCtx n (es ++ [ex]) up ux : cs) ue
    | otherwise = State h (PatMatCtx p : ConMatCtx n es p us : cs) ex

--
-- AppCtx
--

eval (State h (AppCtx [a, b] : cs) (Pop op)) = State h (PopFstCtx op b : cs) a

eval (State h (AppCtx (a : as) : cs) ex)
    | Syntax.isValue ex = State h (AppArgCtx ex as : cs) a

--
-- AppArgCtx
--

eval (State h (AppArgCtx (Con n es) as : cs) ex) =
    State h cs (Con n (es ++ [ex] ++ as))

eval (State h (AppArgCtx (Lam n (p:ps) bd) ag : cs) ex)
    | Match.matchable ex p =
        let bs       = fromMaybe
                           (error "argument does not match lambda parameter")
                           (Match.match ex p)
            (as, h1) = Heap.allocWithName bs h
            bb       = Bind.bind [] as bd
        in  case ag of
                []     -> State h1 cs (if null ps then bb else Lam Nothing ps bb)
                (e:es) -> State h1 (AppArgCtx (Lam Nothing ps bb) es : cs) e
    | otherwise = State h (PatMatCtx p : AppArgCtx (Lam n (p:ps) bd) ag : cs) ex

eval (State _ (AppArgCtx (Lam _ [] _) _ : _) _) =
    error "too many arguments provided to lambda"

eval (State _ (AppArgCtx fn _ : _) _)
    | Syntax.isValue fn = error "application to unexpected value"

--
-- PopFstCtx
--

eval (State h (PopFstCtx op rhs : cs) lhs)
    | Syntax.isValue lhs = State h (PopSndCtx op lhs : cs) rhs

--
-- PopSndCtx
--

eval (State h (PopSndCtx op lhs : cs) rhs)
    | Syntax.isValue rhs = State h cs (primOp op lhs rhs)

--
-- IteCtx
--

-- TODO: again, a sanity-check for all values might work better
eval (State h (IteCtx lt rt : cs) (Log c)) =
    State h cs (if c then lt else rt)

--
-- CasMatCtx
--

eval (State _ (CasMatCtx [] : _) _) =
    error "exhausted all options in case"

eval (State h (CasMatCtx (Alt p mg ex : rs) : cs) c)
    | Match.matchable c p =
        -- Check for pattern match
        case Match.match c p of
            Just ms ->
                -- Check for case guard
                case mg of
                    Just gd -> State h (CasGrdCtx p ex c rs : cs) gd
                    Nothing ->
                        let (bs, h1) = Heap.allocWithName ms h
                            bb       = Bind.bind [] bs ex
                        in  State h1 cs bb
            Nothing -> State h (CasMatCtx rs : cs) c
    | otherwise =
        State h (PatMatCtx p : CasMatCtx (Alt p mg ex : rs) : cs) c

--
-- CasGrdCtx
--

-- TODO: again, a sanity-check for all values would be nice
eval (State h (CasGrdCtx p ex c rs : cs) (Log g))
    | g =
        let ms       = fromJust (Match.match c p)
            (bs, h1) = Heap.allocWithName ms h
            bb       = Bind.bind [] bs ex
        in  State h1 cs bb
    | otherwise = State h (CasMatCtx rs : cs) c

eval (State _ (CasGrdCtx{} : _) ex)
    | Syntax.isValue ex = error "non-boolean value returned by case guard"

--
-- RefCtx
--

eval (State h (RefCtx a : cs) ex)
    | Syntax.isValue ex = State (Heap.update a ex h) cs ex

--
-- Continuations
--

eval (State h cs (App f as))  = State h (AppCtx as : cs) f
eval (State h cs (Ite c l r)) = State h (IteCtx l r : cs) c
eval (State h cs (Cas c bs))  = State h (CasMatCtx bs : cs) c
eval (State h cs (Ref v a))   = State h (RefCtx a : cs) (snd (Heap.deref a h))
eval s@(State _ _ (Let _ _))  = Let.eval s

--
-- Catch-all
--

eval s
    | isFinal s = s
    | otherwise = error $ "no evaluation strategy for state\n" ++ show s
