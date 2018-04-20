module Skiptracer.Eval (
    eval,
    fromExp,
    isFinal
) where

import           Data.Maybe            (fromMaybe)
import qualified Skiptracer.Eval.Bind  as Bind
import           Skiptracer.Eval.Data  (Ctx (..), State (..))
import qualified Skiptracer.Eval.Let   as Let
import qualified Skiptracer.Eval.Match as Match
import           Skiptracer.Heap       (Heap (..))
import qualified Skiptracer.Heap       as Heap
import           Skiptracer.Syntax     (Alt (..), Exp (..), Pat (..))
import qualified Skiptracer.Syntax     as Syntax

isFinal :: State -> Bool
isFinal (State _ [] e) | Syntax.isRef e   = False
isFinal (State _ [] e) | Syntax.isValue e = True
isFinal _              = False

fromExp :: Exp -> State
fromExp e = State Heap.empty [] e

-- | Check if a function application is a primitive operation.
isPrimOp :: String -> Bool
isPrimOp = (`elem` ["+", "-", "*", "<", ">", "<=", ">=", "==", "/="])

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
-- Ref
-- Goes first, because Syntax.isValue would fail otherwise.
--

eval (State h cs (Ref v a)) =
    State h cs (Shr a (snd (Heap.deref a h)))

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
-- TODO: add case for single argument primitive operations

eval (State h (AppCtx [a, b] : cs) (Var op))
    | isPrimOp op = State h (PopFstCtx op b : cs) a

eval (State h (AppCtx (a : as) : cs) ex)
    | Syntax.isValue ex = State h (AppArgCtx ex as : cs) a

--
-- AppArgCtx
--

eval (State h (AppArgCtx (Con n es) as : cs) ex) =
    State h cs (Con n (es ++ [ex] ++ as))

eval (State h (AppArgCtx (Lam (p:ps) bd) ag : cs) ex)
    | Match.matchable ex p =
        let bs       = fromMaybe
                           (error "argument does not match lambda parameter")
                           (Match.match ex p)
            (as, h1) = Heap.allocWithName bs h
            bb       = Bind.bind [] as bd
        in  case ag of
                []     -> State h1 cs (if null ps then bb else Lam ps bb)
                (e:es) -> State h1 (AppArgCtx (Lam ps bb) es : cs) e
    | otherwise = State h (PatMatCtx p : AppArgCtx (Lam (p:ps) bd) ag : cs) ex

eval (State _ (AppArgCtx (Lam [] _) _ : _) _) =
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
                    Just gd -> State h (CasGrdCtx ms ex c rs : cs) gd
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
eval (State h (CasGrdCtx ms ex c rs : cs) (Log g))
    | g =
        let (bs, h1) = Heap.allocWithName ms h
            bb       = Bind.bind [] bs ex
        in  State h1 cs bb
    | otherwise = State h (CasMatCtx rs : cs) c

eval (State _ (CasGrdCtx{} : _) ex)
    | Syntax.isValue ex = error "non-boolean value returned by case guard"

--
-- ShrCtx
--

eval (State h (ShrCtx a : cs) ex)
    | Syntax.isValue ex = State (Heap.update a ex h) cs ex

--
-- Continuations
--

eval (State h cs (Shr a ex))  = State h (ShrCtx a : cs) ex
eval (State h cs (App f as))  = State h (AppCtx as : cs) f
eval (State h cs (Ite c l r)) = State h (IteCtx l r : cs) c
eval (State h cs (Cas c bs))  = State h (CasMatCtx bs : cs) c
eval s@(State _ _ (Let _ _))  = Let.eval s

--
-- Catch-All
--

eval s | isFinal s = s
