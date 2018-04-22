module Skiptracer.GC where

import           Data.Maybe        (maybe)
import           Skiptracer.Eval   (Ctx (..), State (..))
import           Skiptracer.Heap   (Heap)
import qualified Skiptracer.Heap   as Heap (deref)
import           Skiptracer.Syntax (Alt (..), Exp (..))

-- | Allows us to walk through the running "stack", as it were.
class Refs a where refs :: a -> [Int]

-- | Short alias for (concatMap . refs)
mapRefs :: Refs a => [a] -> [Int]
mapRefs = concatMap refs

instance Refs State where
    refs (State _ cs e) = mapRefs cs ++ refs e

instance Refs Exp where
    refs (Ref _ i)   = [i]
    refs (Con _ es)  = mapRefs es
    refs (App f es)  = refs f ++ mapRefs es
    refs (Ite c t f) = refs c ++ refs t ++ refs f
    refs (Cas e as)  = refs e ++ mapRefs as
    refs (Let bs e)  = concatMap (refs . snd) bs ++ refs e
    refs _           = []

instance Refs Alt where
    refs (Alt _ me e) = (maybe [] refs me) ++ refs e

instance Refs Ctx where
    refs (ConMatCtx _ es _ us) = mapRefs es ++ concatMap (refs . snd) us
    refs (AppCtx as)           = mapRefs as
    refs (AppArgCtx fn as)     = refs fn ++ mapRefs as
    refs (PopFstCtx _ ex)      = refs ex
    refs (PopSndCtx _ ex)      = refs ex
    refs (IteCtx te fe)        = refs te ++ refs fe
    refs (CasMatCtx as)        = mapRefs as
    refs (CasGrdCtx bs m c as) = concatMap (refs . snd) bs ++ refs m ++ refs c ++ mapRefs as
    refs (RefCtx a)            = [a]
    refs _                     = []

-- | Run a breadth-first search on a heap, returning live references,
-- ensuring we don't fall into a recursive loop by tracking previous
-- matches.
--
-- Probably shouldn't have duplicates (if I haven't made any stupid mistakes).
heapRefs :: [Int] -> [Int] -> Heap Exp -> [Int]
heapRefs []     s _ = s
heapRefs (n:ns) s h
    | n `elem` s = heapRefs ns s h
    | otherwise  = heapRefs (ns ++ refs (snd (Heap.deref n h))) (n : s) h