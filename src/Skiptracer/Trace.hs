module Skiptracer.Trace (
    TraceOpts (..),

    traces,
    display
) where

import           Skiptracer.Eval   (Ctx (..), State (..))
import qualified Skiptracer.Eval   as Eval (eval, isFinal)
import           Skiptracer.Syntax (Exp (..))

-- Technically, all this module does is filter a really long list.

data TraceOpts =
    TraceOpts
    { tDepth :: Int            -- ^ Max context depth
    , tSkip  :: [String]       -- ^ Functions to skip
    , tOnly  :: Maybe [String] -- ^ Functions to keep (takes precendence)
    }

-- | Returns all the evaluation steps from a starting state.
traces :: State -> [State]
traces s
    | Eval.isFinal s = [s]
    | otherwise      = s : traces (Eval.eval s)

-- | Takes a list of subsequent states and only keeps the interesting
-- stuff.
display :: TraceOpts -> [State] -> [State]
display _ []     = []
display o (s:ss) =
    case s of
        (State _ (RefCtx _ : _) _) -> display o ss
        (State _ _ (Ref _ _))      -> display o ss
        _                          -> s : display o ss

-- If app is primOp, we eval until there's the primops were exhausted
-- If app is safe, we eval until the length of the ctx is (length - 1)
-- If nothing interesting happens, we get rid of the item
