module Main where

import           Debug.Trace          (trace)
import           Skiptracer.Eval      (eval, fromExp, isFinal)
import           Skiptracer.Parse     (parse, parseGhc)
import           Skiptracer.Trace     (TraceOpts (..), display, traces)

import           Skiptracer.Eval.Data (State (..))

main :: IO ()
main =
    readFile "examples/map_fold.hs" >>=
    (writeFile "test.txt" . unlines . map show . display undefined . traces . fromExp . parse)
