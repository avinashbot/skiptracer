module Main where

import           Data.List            (intercalate)
import           Data.Maybe           (mapMaybe)
import           Skiptracer.Eval      (eval, fromExp, isFinal)
import           Skiptracer.Eval.Data (State (..))
import           Skiptracer.GC        (mark)
import           Skiptracer.Heap      (deref, get)
import           Skiptracer.Parse     (parse, parseGhc)
import           Skiptracer.Pretty    (prettyPrint)
import           Skiptracer.Syntax    (Exp (..), isValue)
import           Skiptracer.Trace     (Trace (..), TraceOpts (..), trace)
import           Skiptracer.UnEval    (unEvalUpto, unHeap)
import           System.Environment   (getArgs)

showTrace :: Trace -> String
-- showTrace (Trace (State h cs e) _) | Debug.Trace.trace (show (unHeap h $ snd $ unEvalUpto 6 e cs)) False = undefined
showTrace (Trace (State h cs e) trc) =
    let (tr, ep) = unEvalUpto 6 e cs
        uhe      = unHeap [] h ep
        pp       = prettyPrint uhe
        binds    = mark uhe h
        shBinds  = mapMaybe (\(n, a) -> let (_, e) = deref a h in if isValue e then Nothing else Just ("    " ++ n ++ show a ++ " = " ++ prettyPrint (unHeap [] h e))) binds
        bindStr  = if not (null shBinds) then "\nwhere\n" ++ unlines shBinds else "\n"
    in  if tr
        then intercalate "\n" (map ("... " ++) (lines pp)) ++ bindStr ++ "\n" ++ show trc
        else pp ++ bindStr ++ "\n" ++ show trc

main :: IO ()
main = do
    [fname] <- getArgs
    file <- readFile fname
    let state = fromExp (parse file)
    let opts = TraceOpts [] Nothing False True
    let traces = trace opts state

    putStrLn $ intercalate "\n\n" $ map showTrace traces
