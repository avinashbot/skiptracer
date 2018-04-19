module Skiptracer1.Heap (
    Heap,
    empty,
    allocMany,
    namedAlloc,
    deref,
    update
) where

import           Data.Maybe      (fromMaybe)
import           Skiptracer1.AST (Exp (..))

-- | The heap stores expressions at integer addresses.
-- In a value Heap n f aes, n is the number of expressions
-- stored in the heap, f is the free-list of unallocated addresses
-- and aes is the association-list of addresses with expressions.
data Heap = Heap Int [Int] [(Int, Exp)]

instance Show Heap where
    -- | Print the heap size and heap contents.
    show (Heap size _ aes) = "Heap (" ++ show size ++ "): " ++ show aes

-- | Create a new empty heap.
empty :: Heap
empty = Heap 0 [1..] []

-- | Allocate a single expression onto the heap.
-- Returns the allocated heap address and new heap.
-- NOTE: a possible optimization is dereferencing Refs.
allocOne :: Exp -> Heap -> (Int, Heap)
allocOne e (Heap n (a:f) aes) = (a, Heap (n + 1) f ((a, e) : aes))

-- | Allocate a bunch of expressions onto the heap.
-- Returns the allocated addresses and the new heap.
allocMany :: [Exp] -> Heap -> ([Int], Heap)
allocMany []     h = ([], h)
allocMany (e:es) h = let (a,  h1) = allocOne e h
                         (as, hn) = allocMany es h1
                     in (a:as, hn)

-- | Helper function to assign named expressions onto the heap.
namedAlloc :: [(String, Exp)] -> Heap -> ([(String, Int)], Heap)
namedAlloc binds heap =
    (zip names addrs, heap1)
  where
    (names, exps) = unzip binds
    (addrs, heap1) = allocMany exps heap

-- | Lookup an expression from the heap by its address.
-- NOTE: A Maybe might work better than an error in the long term.
lookupExp :: Int -> Heap -> Exp
lookupExp a (Heap _ _ aes) =
    fromMaybe
        (error $ "lookup address not in heap: " ++ show a)
        (lookup a aes)

-- | De-referencing of heap addresses. The result also includes
-- the final address in the chain before a non-Ref expression is reached.
deref :: Int -> Heap -> (Int, Exp)
deref a h = case lookupExp a h of
                Ref _ a1 -> deref a1 h
                e        -> (a, e)

-- | Update the value of a Heap expression.
update :: Int -> Exp -> Heap -> Heap
update a e (Heap n f aes) = Heap n f (upd aes)
  where
    upd []     = error $ "update address not in heap: " ++ show a
    upd (x:xs) = if fst x == a then (a, e) : xs else x : upd xs
