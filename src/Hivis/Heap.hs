module Hivis.Heap (
    Heap,
    empty,
    allocMany,
    deref,
    update
) where

import           Data.Maybe (fromMaybe)
import           Hivis.AST  (Exp (..))

-- | The heap stores expressions at integer addresses.
-- In a value Heap n f aes, n is the number of expressions
-- stored in the heap, f is the free-list of unallocated addresses
-- and aes is the association-list of addresses with expressions.
data Heap a = Heap Int [Int] [(Int, a)]

instance Show a => Show (Heap a) where
    -- | Print the heap size and heap contents.
    show (Heap size _ exps) = "Heap (" ++ show size ++ "): " ++ show exps

-- | Create a new empty heap.
empty :: Heap a
empty = Heap 0 [1..] []

-- | Allocate a single expression onto the heap.
-- Returns the allocated heap address and new heap.
allocOne :: a -> Heap a -> (Int, Heap a)
allocOne e (Heap n (a:f) aes) = (a, Heap (n+1) f ((a,e):aes))

-- | Allocate a bunch of expressions onto the heap.
-- Returns the allocated addresses and the new heap.
-- NOTE: a possible optimization is dereferencing Refs.
allocMany :: [a] -> Heap a -> ([Int], Heap a)
allocMany []     h = ([],h)
allocMany (e:es) h = let (a,  h1) = allocOne e h
                         (as, hn) = allocMany es h1
                     in (a:as, hn)

-- | Lookup an expression from the heap by its address.
lookupExp :: Int -> Heap a -> a
lookupExp a (Heap _ _ aes) =
    fromMaybe
        (error $ "lookup address not in heap: " ++ show a)
        (lookup a aes)

-- | De-referencing of heap addresses. The result also includes
-- the final address in the chain before a non-Ref expression is reached.
deref :: Int -> Heap Exp -> (Int, Exp)
deref a h = case lookupExp a h of
                Ref _ a1 -> deref a1 h
                e        -> (a,e)

-- | Update the value of a Heap expression.
update :: Int -> a -> Heap a -> Heap a
update a e (Heap n f aes) = Heap n f (upd aes)
  where
    upd []     = error $ "update address not in heap: " ++ show a
    upd (x:xs) = if fst x == a then (a, e) : xs else x : upd xs
