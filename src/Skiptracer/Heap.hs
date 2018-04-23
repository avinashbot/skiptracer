module Skiptracer.Heap (
    Heap,

    empty,

    get,
    deref,

    update,
    adjust,

    deallocOne,
    deallocMany,

    allocOne,
    allocMany,
    allocWithName
) where

import           Control.Arrow     (second)
import           Data.Maybe        (fromMaybe, maybe)
import           Skiptracer.Syntax (Exp (..))
import qualified Skiptracer.Syntax as Syntax

-- | The heap stores expressions at integer addresses.
-- In a value Heap n f aes, n is the number of expressions
-- stored in the heap, f is the free-list of unallocated addresses
-- and aes is the association-list of addresses with expressions.
data Heap a = Heap Int [Int] [(Int, a)]

instance (Show a) => Show (Heap a) where
    -- | Print the heap size and heap contents.
    -- show (Heap s _ _) = "Heap (" ++ show s ++ ")"
    show (Heap size _ aes) =
        "Heap (" ++ show size ++ "):" ++ (concatMap (("\n  " ++) . show) aes)

-- | Create a new empty heap.
empty :: Heap a
empty = Heap 0 [1..] []

get :: Int -> Heap a -> a
get a (Heap _ _ aes) =
    case lookup a aes of
        Nothing -> error $ "cannot locate address in heap: " ++ show a
        Just e  -> e

-- | De-referencing of heap addresses. The result also includes
-- the final address in the chain before a non-Ref expression is reached.
deref :: Int -> Heap Exp -> (Int, Exp)
deref a h =
    case get a h of
        Ref _ a1 -> deref a1 h
        e        -> (a, e)

-- | Update the value of a Heap expression.
update :: Int -> a -> Heap a -> Heap a
update a e (Heap n f aes) = Heap n f (upd aes)
  where
    upd []     = error $ "update address not in heap: " ++ show a
    upd (x:xs) = if fst x == a then (a, e) : xs else x : upd xs

-- | Updates the value of a Heap expression using a function.
adjust :: (a -> a) -> Int -> Heap a -> Heap a
adjust f a h = update a (f (get a h)) h

-- | Delete something from the heap.
deallocOne :: Int -> Heap a -> Heap a
deallocOne a (Heap s fs aes) = Heap (s - 1) (a : fs) (del aes)
  where
    del []          = error $ "could not find heap address: " ++ (show a)
    del ((x, y):xs) | x == a    = xs
                    | otherwise = (x, y) : del xs

-- | Deallocate multiple addresses.
deallocMany :: [Int] -> Heap a -> Heap a
deallocMany []     h = h
deallocMany (a:as) h = deallocMany as (deallocOne a h)

-- | Allocate a single expression onto the heap.
-- Returns the allocated heap address and new heap.
-- NOTE: a possible optimization is dereferencing Refs.
allocOne :: a -> Heap a -> (Int, Heap a)
allocOne e (Heap n (a:f) aes) = (a, Heap (n + 1) f ((a, e) : aes))

-- | Allocate a bunch of expressions onto the heap.
-- Returns the allocated addresses and the new heap.
allocMany :: [a] -> Heap a -> ([Int], Heap a)
allocMany []     h = ([], h)
allocMany (e:es) h = let (a,  h1) = allocOne e h
                         (as, hn) = allocMany es h1
                     in (a:as, hn)

-- | A helper function that returns a lookup list from names to addresses.
allocWithName :: [(String, a)] -> Heap a -> ([(String, Int)], Heap a)
allocWithName ns h0 =
    (zip names addrs, h1)
  where
    (names, exps) = unzip ns
    (addrs, h1)   = allocMany exps h0
