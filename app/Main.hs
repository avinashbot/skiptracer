module Main where

import           Data.List            (intercalate)
import           Data.Maybe           (mapMaybe)
import           Skiptracer.Eval      (eval, fromExp, isFinal)
import           Skiptracer.Eval.Data (State (..))
import           Skiptracer.GC        (mark)
import           Skiptracer.Heap      (deref)
import           Skiptracer.Parse     (parse, parseGhc)
import           Skiptracer.Pretty    (prettyPrint)
import           Skiptracer.Syntax    (isValue)
import           Skiptracer.Trace     (Trace (..), TraceOpts (..), trace)
import           Skiptracer.UnEval    (unEvalUpto, unHeap)

showTrace :: Trace -> String
showTrace (Trace (State h cs e) trc) =
    let (tr, ep) = unEvalUpto 6 e cs
        uhe      = unHeap h ep
        pp       = prettyPrint uhe
        binds    = mark uhe h
        shBinds  = unlines $ mapMaybe (\(n, a) -> let (_, e) = deref a h in if isValue e then Nothing else Just ("    " ++ n ++ show a ++ " = " ++ prettyPrint (unHeap h e))) binds
        bindStr  = if not (null binds) then "\nwhere\n" ++ shBinds else "\n"
    in  if tr
        then intercalate "\n" (map ("... " ++) (lines pp)) ++ bindStr ++ "\n" ++ show trc
        else pp ++ bindStr ++ "\n" ++ show trc

main :: IO ()
main = do
    file <- readFile "examples/map_fold.hs"
    let state = fromExp (parse file)
    let opts = TraceOpts [] Nothing True True
    let traces = trace opts state

    putStrLn $ intercalate "\n\n" $ map showTrace traces
