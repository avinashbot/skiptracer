module Skiptracer.Trace (
    TraceOpts (..),
    Trace (..),
    TraceAction (..),

    trace
) where

import           Data.List             (intercalate)
import           Data.Maybe            (fromJust, isJust, isNothing, maybe)
import           Skiptracer.Eval       (Ctx (..), State (..))
import qualified Skiptracer.Eval       as Eval
import qualified Skiptracer.Eval.Match as Match
import           Skiptracer.Syntax     (Alt (..), Exp (..))
import qualified Skiptracer.Syntax     as Syntax

-- Technically, all this module does is filter a really long list.

data TraceOpts =
    TraceOpts
    { skip       :: [String]       -- ^ Functions to skip
    , only       :: Maybe [String] -- ^ Functions to keep (takes precendence)
    , implPatMat :: Bool           -- ^ Implicit pattern matching evaluation
    , aggrPrimOp :: Bool           -- ^ Aggregate primOps
    }

data Trace =
    Trace
    { traceFocus :: State
    , nextAction :: TraceAction
    }

data TraceAction
    = TraceAppFn String
    | TraceAppPrimOps [String]
    | TraceAppLam
    | TraceEnd

instance Show TraceAction where
    show (TraceAppFn f)        = "{ applying " ++ f ++ " }"
    show (TraceAppPrimOps ops) = "{ applying " ++ intercalate ", " ops ++ " }"
    show TraceAppLam           = "{ applying lambda }"
    show TraceEnd              = "{ final state }"

-- | Returns all the evaluation steps from a starting state.
steps :: State -> [State]
steps s = s : if Eval.isFinal s then [] else steps (Eval.eval s)

trace :: TraceOpts -> State -> [Trace]
trace t s = traceAll t (steps s)

traceAll :: TraceOpts -> [State] -> [Trace]
traceAll t [] = []
traceAll t (a:b:bs)
    -- Skip if we're supposed to skip pattern matching.
    | implPatMat t && inPatMatCtx a = traceAll t (b:bs)
    -- Trace if we're supposed to trace this function (by name).
    | isJust traceFn && shouldTraceFn t (fromJust traceFn) =
        Trace b (TraceAppFn (fromJust traceFn)) : traceAll t bs
    | willAnonApp a = Trace b TraceAppLam : traceAll t bs
    | isJust traceOp && aggrPrimOp t =
        let (p, np) = span (isJust . willPrimOp) (a:b:bs)
        in  Trace a (TraceAppPrimOps (map (fromJust . willPrimOp) p)) : traceAll t np
    | isJust traceOp = Trace a (TraceAppPrimOps [fromJust traceOp]) : traceAll t (b:bs)
    | otherwise      = traceAll t (b:bs)
  where
    traceFn = willNamedApp a
    traceOp = willPrimOp a

inPatMatCtx :: State -> Bool
inPatMatCtx (State _ cs _) =
    any isPatMatCtx cs
  where
    isPatMatCtx (PatMatCtx _) = True
    isPatMatCtx _             = False

willNamedApp :: State -> Maybe String
willNamedApp (State _ (AppArgCtx (Lam (Just f) (p:_) _) [] : _) e)
    | Match.matchable e p = Just f
willNamedApp (State _ (AppArgCtx (Con n _) [] : _) _) = Just n
willNamedApp _ = Nothing

shouldTraceFn :: TraceOpts -> String -> Bool
shouldTraceFn t f = f `notElem` skip t && maybe True (f `elem`) (only t)

willAnonApp :: State -> Bool
willAnonApp (State _ (AppArgCtx (Lam Nothing (p:_) _) [] : _) e) = Match.matchable e p

willPrimOp :: State -> Maybe String
willPrimOp (State _ (PopSndCtx o _ : _) e) | Syntax.isValue e = Just o
willPrimOp _                               = Nothing

-- If app is primOp, we eval until there's the primops were exhausted
-- If app is safe, we eval until the length of the ctx is (length - 1)
-- If nothing interesting happens, we get rid of the item
