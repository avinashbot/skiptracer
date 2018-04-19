-- | Evaluates Let-expressions in the context of a Heap.
--
-- Evaluating Let expressions consists of the following steps:
--   1. Allocate the let expressions onto the heap.
--   2. If any of the expressions are complex patterns, we allocate
--      the individual pattern variables onto the heap.
--   3. We update the let expressions using bind.
--   4. We finally update the in-expression.

module Skiptracer.Eval.Let (eval) where

import qualified Skiptracer.Eval.Bind as Bind
import           Skiptracer.Eval.Data (State (..))
import           Skiptracer.Heap      (Heap)
import qualified Skiptracer.Heap      as Heap
import           Skiptracer.Syntax    (Exp (..), Pat (..))
import qualified Skiptracer.Syntax    as Syntax

-- | Returns the pattern's "name".
-- For all named patterns, (patName p : heapBinds p) == Syntax.bindings p
patName :: Pat -> String
patName (PVar n)   = n
patName (PPat n _) = n
patName _          = "_"

-- | Fetch all non-top-level bindings.
heapBinds :: Pat -> [String]
heapBinds (PVar _)   = []
heapBinds (PPat _ p) = Syntax.bindings p
heapBinds p          = Syntax.bindings p

-- | Return an expression that "extracts" a variable from a pattern from an
-- expression.
patExp :: Int -> String -> Pat -> Exp
patExp a v p = App (Lam [p] (Var v)) [Ref v a]

-- | Allocates the let statements and their patterns to the heap.
allocLet :: (Pat, Exp) -> ([Int], [(String, Int)], Heap Exp) -> ([Int], [(String, Int)], Heap Exp)
allocLet (p, e) (as, fs, h0) =
    (pa : as, (pn, pa) : gs ++ fs, h2)
  where
    (pn, hb) = (patName p, heapBinds p)
    (pa, h1) = Heap.allocOne e h0
    (gs, h2) = Heap.allocWithName (map (\s -> (s, patExp pa s p)) hb) h1

-- | Rebind heap lets with the new binds.
updateHeapLets :: [Int] -> [(String, Int)] -> Heap Exp -> Heap Exp
updateHeapLets []     _  h = h
updateHeapLets (l:ls) bs h = let h1 = Heap.adjust (Bind.bind [] bs) l h
                             in  updateHeapLets ls bs h

-- | Eval function that specializes in Lets.
eval :: State -> State
eval (State h0 cs (Let bs e0)) =
    let (as, fs, h1) = foldr allocLet ([], [], h0) bs
        h2           = updateHeapLets as fs h1
        e1           = Bind.bind [] fs e0
    in  State h2 cs e1
eval _ = error "Let.eval does not define eval for non-let expressions"
