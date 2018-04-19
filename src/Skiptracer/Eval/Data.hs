module Skiptracer.Eval.Data (State (..), Ctx (..)) where

import           Skiptracer.Heap   (Heap)
import           Skiptracer.Syntax (Exp, Grd, Pat)

-- | Data constructor for the interpreter state.
data State = State (Heap Exp) [Ctx] Exp deriving Show

-- | Interpreter context. Kinda acts like a stack.
-- TODO: Double-check haddock documentation.
data Ctx

    -- | Evaluates the expression until it's pattern-matchable. We're not
    -- binding variables yet, we're just evaluating to a point where we can.
    --
    -- PVars and PWlds don't need to be evaluated, but the others do.
    -- If we hit a PCon inside the pattern, we put a PConMatCtx onto
    -- the stack after we get a Con (only if the Con names match).
    --
    -- Once fully evaluated, it lets the remaining contexts continue.
    = PatMatCtx
        Pat -- ^ The (non-lazy) pattern to match against.

    -- | Evaluates individual matches inside a PCon specifically.
    -- The actual work is done by PatMatCtx, this just holds information.
    --
    -- When fully evaluated, it should return the fully-evaluated Con,
    -- throwing away the pattern information.
    | ConMatCtx
        String       -- ^ The constructor we're matching
        [Exp]        -- ^ Fully-evaluated expressions
        Pat          -- ^ The pattern we're matching
        [(Pat, Exp)] -- ^ Unmatched pattern-expression pairs

    -- | Evaluates an App body until we have a Lam value.
    -- NOTE: Possibly also support Cons (since they can be partial)
    --
    -- If fully evaluated, it will swap with AppArgCtx.
    | AppLamCtx
        [Exp] -- ^ The function arguments

    -- | Evaluates an application argument, pattern matches it, and binds
    -- the resulting variables to the lambda function body.
    --
    -- Returns a Lam (if partially evaluated) or a fully bound Exp.
    | AppArgCtx
        Exp   -- ^ The lambda
        [Exp] -- ^ The remaining arguments

    -- | Since primitive operations require values, we're evaluating the
    -- first (left) expression of the operation.
    --
    -- Once evaluated, it replaces itself with PopSndCtx
    | PopFstCtx
        String -- ^ The operation name
        Exp    -- ^ The second (right) expression

    -- | Once the first expression is fully evaluated, we evaluate the second
    -- one.
    --
    -- Once completed, it calls primOp on the two expressions.
    | PopSndCtx
        String -- ^ The operation name
        Exp    -- ^ The (now fully-evaluated) first expression

    -- | Evaluating the condition in an if expression.
    --
    -- When the expression is evaluated, the resulting boolean determines
    -- which expression is evaluated next.
    | IteCtx
        Exp -- ^ True expression
        Exp -- ^ False expression

    -- | Case condition evaluation. When adding this context to the stack,
    -- it must always be followed by a PatMatCtx to make sure we can pattern-
    -- match correctly. If we get to the end without matching, we error out.
    --
    -- If we don't match, we move on to the next one.
    -- If we match, but there's a guard, we swap to CasGrdCtx.
    -- If we match, and there isn't a guard, we return that expression.
    | CasMatCtx
        [(Grd Pat, Exp)] -- ^ Alts

    -- | Case guard evaluation. Basically the same as CasMatCtx, since we may
    -- need to go back to it in case the guard fails. It is assumed that the
    -- relevant guarded pattern has a non-maybe guard expression.
    --
    -- If it fails, we go back to evaluating the next expression in CasMatCtx.
    | CasGrdCtx
        [(String, Exp)]  -- ^ Matched bindings
        Exp              -- ^ Current branch
        Exp              -- ^ Case condition (in case of failure)
        [(Grd Pat, Exp)] -- ^ Alts (in case of failure)

    -- | Updates a ref with the result of an evaluation to enable sharing.
    --
    -- We also need a special Exp for this because some patterns (eg PVar)
    -- don't even need to evaluate to match something so we should evaluate
    -- lazily as well.
    | ShrCtx
        Int -- ^ The heap address to update

    deriving Show

