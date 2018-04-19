module Skiptracer.Eval (
    eval
) where

import           Skiptracer.Eval.Data (Ctx (..), State (..))
import qualified Skiptracer.Eval.Let  as Let
import           Skiptracer.Heap      (Heap (..))
import qualified Skiptracer.Heap      as Heap
import           Skiptracer.Syntax    (Exp (..), Grd (..), Pat (..))
import qualified Skiptracer.Syntax    as Syntax

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

--
--
-- Eval and Continue
-- NOTE: Maybe we could check whether eval is run and just skip tracing it,
--   because the only substantative change is caused by kont.
--
--

eval :: State -> State
-- PatMatCtx
-- ConMatCtx
-- AppCtx
-- AppArgCtx
-- PopFstCtx
-- PopSndCtx
-- IteCtx
-- CasMatCtx
-- CasGrdCtx
-- RefCtx

eval (State h cs (App f as))     = State h (AppCtx as : cs) f
eval (State h cs (Pop o l r))    = State h (PopFstCtx o r : cs) l
eval (State h cs (Ite c l r))    = State h (IteCtx l r : cs) c
eval (State h cs (Ref v a))      = State h (RefCtx a : cs) (snd (Heap.deref a h))
eval (State h cs (Cas c (b:bs))) = State h (CasMatCtx [] b bs : cs) c
eval s@(State _ _ (Let _ _))     = Let.eval s
