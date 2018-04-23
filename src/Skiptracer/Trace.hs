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

-- FIXME: Absolute mess.
-- traceAll :: TraceOpts -> [State] -> [Trace]
-- -- traceAll t xs = map (`Trace` TraceEnd) xs
-- traceAll t [a] = [Trace a TraceEnd]
-- traceAll t (a:b:bs)
--     -- Skip if we're supposed to skip pattern matching.
--     | skipPatMat t && inPatMatCtx a = traceAll t (b:bs)
--     -- Trace if we're supposed to trace this function (by name).
--     | isJust traceFn && shouldTraceFn t (fromJust traceFn) =
--         Trace a (TraceAppFn (fromJust traceFn)) : traceAll t (b:bs)
--     | isJust traceOp && aggrPrimOp t =
--         let (p, np) = span (isJust . willPrimOp) (a:b:bs)
--         in  Trace a (TraceAppPrimOps (map (fromJust . willPrimOp) p)) : traceAll t np
--     | isJust traceOp = Trace a (TraceAppPrimOps [fromJust traceOp]) : traceAll t (b:bs)
--     | isJust traceOt = Trace b (fromJust traceOt) : traceAll t bs
--     | isJust traceEx = Trace a (fromJust traceEx) : traceAll t (b:bs)
--     | otherwise      = traceAll t (b:bs)
--   where
--     traceFn = willNamedApp a
--     traceOp = willPrimOp a
--     traceOt = willTraceable a
--     traceEx = isTraceable a

traceAll :: TraceOpts -> [State] -> [Trace]
-- Final state.
traceAll t [a]    = [Trace a TraceEnd]
-- Evaluation occuring during pattern matching
traceAll t (a:as) | skipPatMat t && inPatMatCtx a = traceAll t as
-- Prim Ops
-- traceAll t (a@(State _ _ (App (Var n) [e1, e2])):as)
traceAll t (a@(State _ (PopSndCtx n _ : _) e):as)
    | Syntax.isPrimOp n && shouldTraceFn t n && Syntax.isValue e =
        let (tr@(Trace _ nt) : trs) = traceAll t as
        in  case nt of
            (TraceAppPrimOps ops) -> Trace a (TraceAppPrimOps (n : ops)) : trs
            _                     -> Trace a (TraceAppPrimOps [n]) : tr : trs
-- Function application
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

willNamedApp :: State -> Maybe String
willNamedApp (State _ (AppArgCtx (Lam (Just f) (p:_) _) [] : _) e)
    | Match.matchable e p = Just f
willNamedApp (State _ (AppArgCtx (Con n _) [] : _) _) = Just n
willNamedApp _ = Nothing

shouldTraceFn :: TraceOpts -> String -> Bool
shouldTraceFn t f = f `notElem` skip t && maybe True (f `elem`) (only t)

isTraceable :: State -> Maybe TraceAction
isTraceable (State _ _ Ite{}) = Just TraceIte
isTraceable (State _ _ Cas{}) = Just TraceCas
isTraceable _                 = Nothing

willTraceable :: State -> Maybe TraceAction
willTraceable (State _ (AppArgCtx (Lam _ (p:_) _) [] : _) e) | Match.matchable e p = Just TraceAppLam
willTraceable _ = Nothing

willPrimOp :: State -> Maybe String
willPrimOp (State _ (PopSndCtx o _ : _) e) | Syntax.isValue e = Just o
willPrimOp _                               = Nothing

-- If app is primOp, we eval until there's the primops were exhausted
-- If app is safe, we eval until the length of the ctx is (length - 1)
-- If nothing interesting happens, we get rid of the item
