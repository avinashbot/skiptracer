module Main where

import           Debug.Trace          (trace)
import           Skiptracer.Eval      (eval, fromExp, isFinal)
import           Skiptracer.Parse     (parse, parseGhc)

import           Skiptracer.Eval.Data (State (..))

main :: IO ()
main = do
    file <- readFile "examples/map_fold.hs"
    print $ until isFinal (\s -> trace (show s) (eval s)) (fromExp (parse file))
