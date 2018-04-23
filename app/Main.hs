module Main where

import           Debug.Trace          (trace)
import           Skiptracer.Eval      (eval, fromExp, isFinal)
import           Skiptracer.Parse     (parse, parseGhc)
import           Skiptracer.Trace     (TraceOpts (..), filterTrace, traces)

import           Skiptracer.Eval.Data (State (..))

main :: IO ()
main =
    readFile "examples/fib.hs" >>=
    (writeFile "test.txt" . unlines . map show . filterTrace undefined . traces . fromExp . parse)
