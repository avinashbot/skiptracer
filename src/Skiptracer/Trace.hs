module Skiptracer.Trace (
    TraceOpts (..),

    traces,
    filterTrace
) where

import           Skiptracer.Eval   (Ctx (..), State (..))
import qualified Skiptracer.Eval   as Eval
import           Skiptracer.Syntax (Exp (..))

-- Technically, all this module does is filter a really long list.

data TraceOpts =
    TraceOpts
    { tMaxDepth :: Int            -- ^ Max context depth
    , tSkip     :: [String]       -- ^ Functions to skip
    , tOnly     :: Maybe [String] -- ^ Functions to keep (takes precendence)
    , tImplicit :: Bool           -- ^ Implicit pattern matching evaluation
    }

-- | Returns all the evaluation steps from a starting state.
traces :: State -> [State]
traces s
    | Eval.isFinal s = [s]
    | otherwise      = s : traces (Eval.eval s)

-- | Takes a list of subsequent states and only keeps the interesting
-- stuff.
filterTrace :: TraceOpts -> [State] -> [State]
filterTrace _ []     = []
filterTrace o (s:ss) =
    case s of
        (State _ (RefCtx _ : _) _)    -> filterTrace o ss
        (State _ (PatMatCtx{} : _) _) -> filterTrace o ss
        (State _ (ConMatCtx{} : _) _) -> filterTrace o ss
        (State _ _ (Ref _ _))         -> filterTrace o ss
        _                             -> s : filterTrace o ss

-- | Truncate the contexts to `s` meaningful contexts.
-- "Meaningful" in the context of translating to Exp.
trunc :: Int -> [Ctx] -> (Bool, [Ctx])
trunc s cs =
    if   s < length dcs
    then (True, take s cs)
    else (False, cs)
  where
    dcs = filter meaningful cs
    meaningful (RefCtx _)    = False
    meaningful (PatMatCtx _) = False
    meaningful _             = True

-- If app is primOp, we eval until there's the primops were exhausted
-- If app is safe, we eval until the length of the ctx is (length - 1)
-- If nothing interesting happens, we get rid of the item
