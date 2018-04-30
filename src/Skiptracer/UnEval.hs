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
unEvalUpto n e (NrmCtx : cs)      = unEvalUpto n e cs
unEvalUpto n e (c : cs)           = unEvalUpto (n - 1) (unCtx e c) cs

-- | Remove value expression from heap and reinsert them directly into the
-- expression.
unHeap :: [Int] -> Heap Exp -> Exp -> Exp
unHeap s h (Con n es)   = Con n (map (unHeap s h) es)
unHeap s h (Lam n ps e) = Lam n ps (unHeap s h e)
unHeap s h (App e as)   = App (unHeap s h e) (map (unHeap s h) as)
unHeap s h (Ite c t f)  = Ite (unHeap s h c) (unHeap s h t) (unHeap s h f)
unHeap s h (Cas e as)   = Cas (unHeap s h e) (map (unHeapAlt s h) as)
unHeap s h (Let bs e)   = Let (map (\(a, b) -> (a, unHeap s h b)) bs) (unHeap s h e)
unHeap s h (Ref n a)    | a `notElem` s && Syntax.isValue e = unHeap (a:s) h e where (_, e) = Heap.deref a h
unHeap _ _ e            = e

-- Variant of unHeap for Alts.
unHeapAlt :: [Int] -> Heap Exp -> Alt -> Alt
unHeapAlt s h (Alt p me e) = Alt p (fmap (unHeap s h) me) (unHeap s h e)

-- Remove references to values from an Exp, excluding recursive references.
unHeap2 :: Heap Exp -> [Int] -> Exp -> ([Int], Exp)
unHeap2 h s (Ref n a) | a `notElem` ms && Syntax.isValue ue = (ms, ue)
  where
    (ms, ue) = unHeap2 h (a:s) (snd (Heap.deref a h))
unHeap2 _ _ e = ([], e)

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
unCtx e (NrmConCtx s e1 e2)   = Con s (e1 ++ [e] ++ e2)
unCtx e NrmCtx                = e
unCtx e (RefCtx _)            = e
unCtx e (PatMatCtx _)         = e
