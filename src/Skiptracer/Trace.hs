module Skiptracer.Trace where

import           Skiptracer.Eval (State (..))
import qualified Skiptracer.Eval as Eval (eval, isFinal)

-- Technically, all this module does is filter a really long list.

newtype TraceOpts =
    TraceOpts
    { yo :: Bool
    }

-- | Returns all the evaluation steps from a starting state.
traces :: State -> [State]
traces s
    | Eval.isFinal s = [s]
    | otherwise      = s : traces (Eval.eval s)


-- If app is primOp, we eval until there's the primops were exhausted
-- If app is safe, we eval until the length of the ctx is (length - 1)
-- If nothing interesting happens, we get rid of the item
