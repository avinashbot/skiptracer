module Skiptracer.GC (mark) where

import           Data.Maybe        (maybe)
import           Skiptracer.Eval   (Ctx (..))
import           Skiptracer.Heap   (Heap)
import qualified Skiptracer.Heap   as Heap
import           Skiptracer.Syntax (Alt (..), Exp (..))

-- | Take an Exp and return all live references.
mark :: Exp -> Heap Exp -> [(String, Int)]
mark e = heapRefs (refs e) []

-- | Run a breadth-first search on a heap, returning live references,
-- ensuring we don't fall into a recursive loop by tracking previous
-- matches.
--
-- Probably shouldn't have duplicates (if I haven't made any stupid mistakes).
heapRefs :: [(String, Int)] -> [(String, Int)] -> Heap Exp -> [(String, Int)]
heapRefs []     s _ = s
heapRefs (n:ns) s h
    | n `elem` s = heapRefs ns s h
    | otherwise  = heapRefs (ns ++ refs (Heap.get (snd n) h)) (n : s) h

-- | Allows us to walk through the running "stack", as it were.
class Refs a where refs :: a -> [(String, Int)]

-- | Short alias for (concatMap . refs)
mapRefs :: Refs a => [a] -> [(String, Int)]
mapRefs = concatMap refs

instance Refs Exp where
    refs (Ref n i)   = [(n, i)]
    refs (Con _ es)  = mapRefs es
    refs (Lam _ _ e) = refs e
    refs (App f es)  = refs f ++ mapRefs es
    refs (Ite c t f) = refs c ++ refs t ++ refs f
    refs (Cas e as)  = refs e ++ mapRefs as
    refs (Let bs e)  = concatMap (refs . snd) bs ++ refs e
    refs _           = []

instance Refs Alt where
    refs (Alt _ me e) = maybe [] refs me ++ refs e
