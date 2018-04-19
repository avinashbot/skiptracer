module Hivis.Trace (
    TraceOpts (..),

    defaultTraceOpts,
    trace
) where

import           Hivis.AST  (Exp (..), Pat (..))
import qualified Hivis.AST  as AST
import           Hivis.Eval (Ctx (..), State (..))
import           Hivis.Heap (Heap)
import qualified Hivis.Heap as Heap

-- | Options for the trace printer.
data TraceOpts = TraceOpts
                 { maxDepth       :: Int
                 , expandValRefs  :: Bool
                 , compactPrimOps :: Bool
                 } deriving Show

-- | Default tracing options.
defaultTraceOpts :: TraceOpts
defaultTraceOpts = TraceOpts
                   { maxDepth = 99
                   , expandValRefs = True
                   , compactPrimOps = True
                   }

-- | A trace generated from a program's execution.
data NextAction = TraceApp String
                | TraceEnd

instance Show NextAction where
    show (TraceApp fn) = "{applying " ++ fn ++ "}"
    show TraceEnd      = "{end of program}"

-- | A trace step.
data Trace = Trace State NextAction deriving Show

-- | Return a list of traces from the program's execution history.
trace :: TraceOpts -> [State] -> [Trace]
trace opts (s@(State _ _ (App fn _)) : rest) =
    Trace s (TraceApp fn) : trace opts rest
trace opts (s@(State _ _ (Pop op _ _)) : rest) =
    Trace s (TraceApp op) : trace opts rest
trace opts [s] = [Trace s TraceEnd]
trace opts (_ : rest) = trace opts rest
