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
    , skipPatMat :: Bool           -- ^ Implicit pattern matching evaluation
    , aggrPrimOp :: Bool           -- ^ Aggregate primOps
    }

data Trace = Trace State TraceAction

data TraceAction
    = TraceAppFn String
    | TraceAppPrimOps [String]
    | TraceAppLam
    | TraceIte
    | TraceCas
    | TraceEnd
    deriving Eq

instance Show TraceAction where
    show (TraceAppFn f)        = "{ applying " ++ f ++ " }"
    show (TraceAppPrimOps ops) = "{ applying " ++ intercalate ", " ops ++ " }"
    show TraceAppLam           = "{ applying lambda }"
    show TraceIte              = "{ evaluating if }"
    show TraceCas              = "{ evaluating case }"
    show TraceEnd              = "{ final state }"

-- | Returns all the evaluation steps from a starting state.
steps :: State -> [State]
steps s = s : if Eval.isFinal s then [] else steps (Eval.eval s)

trace :: TraceOpts -> State -> [Trace]
trace t s = traceAll t (steps s)

traceAll :: TraceOpts -> [State] -> [Trace]
-- Final state.
traceAll t [a]    = [Trace a TraceEnd]
-- Evaluation occuring during pattern matching
traceAll t (a:as) | skipPatMat t && inPatMatCtx a = traceAll t as
-- Prim Ops
-- traceAll t (a@(State _ _ (App (Var n) [e1, e2])):as)
traceAll t (a@(State _ (PopSndCtx n _ : _) e):as)
    | shouldTraceFn t n && Syntax.isValue e =
        let (tr@(Trace _ nt) : trs) = traceAll t as
        in  case nt of
            (TraceAppPrimOps ops) | aggrPrimOp t -> Trace a (TraceAppPrimOps (n : ops)) : trs
            _                     -> Trace a (TraceAppPrimOps [n]) : tr : trs
-- Function application
traceAll t (a@(State _ (AppCtx _ : _) (Var n)):as)
    | shouldTraceFn t n = Trace a (TraceAppFn n) : traceAll t as
traceAll t (a@(State _ (AppCtx _ : _) (Con n [])):as)
    | shouldTraceFn t n = Trace a (TraceAppFn n) : traceAll t as
traceAll t (a@(State _ (AppCtx _ : _) (Lam (Just n) _ _)):as)
    | shouldTraceFn t n = Trace a (TraceAppFn n) : traceAll t as
traceAll t (a@(State _ (AppCtx _ : _) (Lam Nothing _ _)):as)
    = Trace a TraceAppLam : traceAll t as
-- If and Cases
traceAll t (a@(State _ _ Ite{}):as) = Trace a TraceIte : traceAll t as
traceAll t (a@(State _ _ Cas{}):as) = Trace a TraceCas : traceAll t as
-- Catch-all
traceAll t (_:as) = traceAll t as

inPatMatCtx :: State -> Bool
inPatMatCtx (State _ cs _) =
    any isPatMatCtx cs
  where
    isPatMatCtx (PatMatCtx _) = True
    isPatMatCtx _             = False

shouldTraceFn :: TraceOpts -> String -> Bool
shouldTraceFn t f = f `notElem` skip t && maybe True (f `elem`) (only t)
