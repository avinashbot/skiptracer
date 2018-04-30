module Skiptracer.UnEval (
    unEval,
    unEvalUpto,

    unHeap
) where

import           Control.Arrow     (second)
import           Data.List         (elemIndices, nubBy, partition)

import           Skiptracer.Eval   (Ctx (..), State (..))
import qualified Skiptracer.Eval   as Eval
import qualified Skiptracer.GC     as GC
import           Skiptracer.Heap   (Heap (..))
import qualified Skiptracer.Heap   as Heap
import           Skiptracer.Syntax (Alt (..), Exp (..), Pat (..))
import qualified Skiptracer.Syntax as Syntax



import qualified Debug.Trace

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

unHeapAll :: Heap Exp -> [Int] -> [Exp] -> ([Int], [Exp])
unHeapAll h s []     = ([], [])
unHeapAll h s (e:es) =
    let (s1, e1)  = unHeap h s e
        (s2, es1) = unHeapAll h s es
    in  (s1 ++ s2, e1 : es1)

-- | Remove value expression from heap and reinsert them directly into the
-- expression. Returns the references that it saw. Uses a stoplist.
--
-- NOTE: Oh geez I gotta figure out Monads soon.
unHeap :: Heap Exp -> [Int] -> Exp -> ([Int], Exp)
unHeap h s (Con n es)   = let (ss, es1) = unHeapAll h s es in (ss, Con n es1)
unHeap h s (Lam n ps e) = let (ss, e2) = unHeap h s e in (ss, Lam n ps e2)
unHeap h s (App e as)   = let (ss1, e1)  = unHeap h s e
                              (ss2, as1) = unHeapAll h s as
                          in  (ss1 ++ ss2, App e1 as1)
unHeap h s (Ite c t f)  = let (ss1, c1)  = unHeap h s c
                              (ss2, t1)  = unHeap h s t
                              (ss3, f1)  = unHeap h s f
                          in  (ss1 ++ ss2 ++ ss3, Ite c1 t1 f1)
unHeap h s (Cas e as)   = let (ss1, e1)  = unHeap h s e
                              (ss2, as1) = unHeapAlts h s as
                          in  (ss1 ++ ss2, Cas e1 as1)
unHeap h s (Let bs e)   = let (ss1, bs1) = unHeapLetBs h s bs
                              (ss2, e1)  = unHeap h s e
                          in  (ss1 ++ ss2, Let bs1 e1)
unHeap h s (Ref n a)
    | a `elem` s = ([a], Ref n a)
    | Syntax.isValue e && a `notElem` ss = (ss, e2)
  where
    (_, e)   = Heap.deref a h
    (ss, e2) = unHeap h (a:s) e
unHeap _ _ e            = ([], e)

unHeapLetBs :: Heap Exp -> [Int] -> [(Pat, Exp)] -> ([Int], [(Pat, Exp)])
unHeapLetBs h s []            = ([], [])
unHeapLetBs h s ((p, b) : bs) = let (ss1, b1)  = unHeap h s b
                                    (ss2, bs1) = unHeapLetBs h s bs
                                in  (ss1 ++ ss2, (p, b1) : bs1)

-- Variant of unHeap for Alts.
unHeapAlts :: Heap Exp -> [Int] -> [Alt] -> ([Int], [Alt])
unHeapAlts h s []                = ([], [])
unHeapAlts h s (Alt p me e : as) = let (ss1, me1) = maybe ([], Nothing) (second Just . unHeap h s) me
                                       (ss2, e1)  = unHeap h s e
                                       (ss3, as1) = unHeapAlts h s as
                                   in  (ss1 ++ ss2 ++ ss3, Alt p me1 e1 : as1)

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
