module Skiptracer.UnEval (
    unEval,
    unEvalUpto,

    unHeap
) where

import           Skiptracer.Eval   (Ctx (..), State (..))
import qualified Skiptracer.Eval   as Eval
import           Skiptracer.Heap   (Heap (..))
import qualified Skiptracer.Heap   as Heap
import           Skiptracer.Syntax (Alt (..), Exp (..))
import qualified Skiptracer.Syntax as Syntax

-- | Generate an Exp from an Exp surrounded by Ctxs.
unEval :: Exp -> [Ctx] -> Exp
unEval = foldl unCtx

-- | Uneval upto a certain number of contexts, also returning whether the
-- expression was truncated early.
--
-- RefCtx and PatMatCtx are not counted since they don't touch the expression.
unEvalUpto :: Int -> Exp -> [Ctx] -> (Bool, Exp)
unEvalUpto _ e []                 = (False, e)
unEvalUpto 0 e _                  = (True, e)
unEvalUpto n e (RefCtx _ : cs)    = unEvalUpto n e cs
unEvalUpto n e (PatMatCtx _ : cs) = unEvalUpto n e cs
unEvalUpto n e (c : cs)           = unEvalUpto (n - 1) (unCtx e c) cs

-- | Remove value expression from heap and reinsert them directly into the
-- expression.
unHeap :: Heap Exp -> Exp -> Exp
unHeap h (Con n es)         = Con n (map (unHeap h) es)
unHeap h (Lam (Just n) _ _) = Var n
unHeap h (Lam Nothing ps e) = Lam Nothing ps (unHeap h e)
unHeap h (App e as)         = App (unHeap h e) (map (unHeap h) as)
unHeap h (Ite c t f)        = Ite (unHeap h c) (unHeap h t) (unHeap h f)
unHeap h (Cas e as)         = Cas (unHeap h e) (map (\(Alt p me e) -> Alt p (fmap (unHeap h) me) (unHeap h e)) as)
unHeap h (Let bs e)         = Let (map (\(a, b) -> (a, unHeap h b)) bs) (unHeap h e)
unHeap h (Ref s a)          | Syntax.isValue e = e where (_, e) = Heap.deref a h
unHeap _ e                  = e

-- Variant of unHeap for Alts.
unHeapAlt :: Heap Exp -> Alt -> Alt
unHeapAlt h (Alt p me e) = Alt p (fmap (unHeap h) me) (unHeap h e)

-- | Unwrap an Exp in the context of a Ctx.
unCtx :: Exp -> Ctx -> Exp
unCtx e (ConMatCtx n es _ ps) = Con n (es ++ [e] ++ map snd ps)
unCtx e (AppCtx as)           = App e as
unCtx e (AppArgCtx fn as)     = App fn (e : as)
unCtx e (PopFstCtx op sn)     = App (Pop op) [e, sn]
unCtx e (PopSndCtx op fs)     = App (Pop op) [fs, e]
unCtx e (IteCtx te fe)        = Ite e te fe
unCtx e (CasMatCtx as)        = Cas e as
unCtx e (CasGrdCtx p b c as)  = Cas c (Alt p (Just e) b : as)
unCtx e (RefCtx _)            = e
unCtx e (PatMatCtx _)         = e
