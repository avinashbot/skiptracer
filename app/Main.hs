module Main where

import           Data.List            (intercalate)
import           Data.Maybe           (mapMaybe)
import           Skiptracer.Eval      (eval, fromExp, isFinal)
import           Skiptracer.Eval.Data (State (..))
import           Skiptracer.GC        (mark)
import           Skiptracer.Heap      (deref, get)
import           Skiptracer.Options   (Options (..), getOpts)
import           Skiptracer.Parse     (parse, parseGhc)
import           Skiptracer.Pretty    (prettyPrint)
import           Skiptracer.Syntax    (Exp (..), isValue)
import           Skiptracer.Trace     (Trace (..), TraceOpts (..), trace)
import           Skiptracer.UnEval    (unEvalUpto, unHeap)

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
    opts  <- getOpts
    file  <- readFile . optFileName $ opts
    pFile <-
        case optPreludeFile opts of
            Nothing -> pure file
            Just f  -> fmap (++ "\n" ++ file) (readFile f)
    let state = fromExp . parse $ pFile
    let tOpts = TraceOpts (optHideFuncs opts) (optOnlyFuncs opts) (optSkipPatMat opts) (not (optSplitPrimOps opts))
    let traces = trace tOpts state

    putStrLn $ intercalate "\n\n" $ map showTrace traces
