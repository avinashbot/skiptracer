module Skiptracer1.Eval (State (..), stepEval) where

import           Control.Arrow     (second, (***))
import           Data.List         (concatMap)
import           Data.Maybe        (maybe)

import           Skiptracer1.AST   (Exp (..), Grd (..), Pat (..))
import qualified Skiptracer1.AST   as AST
import           Skiptracer1.Heap  (Heap)
import qualified Skiptracer1.Heap  as Heap
import qualified Skiptracer1.Match as Match

-- | Data constructor for the interpreter state.
data State = State Heap [Ctx] Exp deriving Show

-- | Interpreter context. Kinda acts like a stack.
data Ctx
    = IteCtx Exp Exp                             -- ^ If-Then-Else with the two branches
    | RefCtx Int                                 -- ^ Variable reference "tracker" with address
    | PopLtCtx String Exp                        -- ^ First argument of primitive operation
    | PopRtCtx String Exp                        -- ^ Final argument of primitive operation
    | CasMatCtx (Grd Pat, Exp) [(Grd Pat, Exp)]  -- ^ Match pattern against condition expression
    | CasGrdCtx [(Grd Pat, Exp)] Exp             -- ^ Run case guard (holding the condition expression in case it doesnt match)
    | LetMatCtx         -- ^ Strictly match pattern against expression
        [(String, Int)] -- ^ The fixed recursive bindings.
        Exp             -- ^ The actual expression to evaluate.
        Pat             -- ^ The pattern to match the running expression against.
        [(Pat, Exp)]    -- ^ The remaining pattern binds.
    | AppCtx [Exp]      -- ^ Fetch the lambda/function from a Rec if we need to
    | PatExpCtx Pat     -- ^ Ensures that all the necessary sub-expressions are evaluated before being pattern-matched.
    deriving (Show)

-- | Returns an "explicit match" expression given a variable. See the let step.
-- NOTE: The first two matches are basically optimizations.
matchVar :: String -> Pat -> Exp -> Exp
matchVar v (PVar n) e   | n == v = e
matchVar v (PPat n _) e | n == v = e
matchVar v p e          = Let [(keepVar v p, e)] (Var v)
  where
    keepVar _ PWld       = PWld
    keepVar v (PNum _)   = PWld
    keepVar v (PLog _)   = PWld
    keepVar v (PVar n)   = if n == v then PVar n else PWld
    keepVar v (PPat n p) = if n == v then PPat n (keepVar v p) else p
    keepVar v (PCon n p) = PCon n (map (keepVar v) p)

-- | Make a parsed expression "eval"-able, given a local scope.
--
-- FIXME: Deal with variable shadowing in Lam, Cas, and Let?
-- A potential way to deal with this is to put "dummy" variables in front every
-- time we enter a binding and only replace then when lookup returns non-dummy vars.
--
-- NOTE: another optimization could be to provide a (Map String Exp) and a Heap and
-- only allocate on the heap if actually needed.
instExp :: [(String, Int)] -> Exp -> Exp
instExp vas v@(Var n)      = maybe v (Ref n) (lookup n vas)
instExp vas (Con s es)     = Con s (map (instExp vas) es)
instExp vas (App f es)     = App (instExp vas f) (map (instExp vas) es)
instExp vas (Pop s e1 e2)  = Pop s (instExp vas e1) (instExp vas e2)
instExp vas (Ite ec et ef) = Ite (instExp vas ec) (instExp vas et) (instExp vas ef)
instExp vas (Lam ps ex)    = Lam ps (instExp vas ex)
instExp vas (Cas ec al)    = Cas (instExp vas ec) (map (instGrd vas *** instExp vas) al)
instExp vas (Let ps ex)    = Let (map (second (instExp vas)) ps) (instExp vas ex)
instExp _   ex             = ex

-- | Instantiate guard expressions.
instGrd :: [(String, Int)] -> Grd a -> Grd a
instGrd vas (Grd a me) = Grd a (fmap (instExp vas) me)

-- | Evaluate a primitive operation.
primOp :: String -> Exp -> Exp -> Exp
primOp "+"  (Num m) (Num n) = Num (m + n)
primOp "-"  (Num m) (Num n) = Num (m - n)
primOp "*"  (Num m) (Num n) = Num (m * n)
primOp "<"  (Num m) (Num n) = Log (m < n)
primOp ">"  (Num m) (Num n) = Log (m > n)
primOp "<=" (Num m) (Num n) = Log (m <= n)
primOp ">=" (Num m) (Num n) = Log (m >= n)
primOp "==" (Num m) (Num n) = Log (m == n)
primOp "/=" (Num m) (Num n) = Log (m /= n)
primOp "==" (Log m) (Log n) = Log (m == n)
primOp "/=" (Log m) (Log n) = Log (m /= n)
primOp s    e1      e2      = error $ "invalid primOp: " ++ unwords [show e1, s, show e2]

-- The grand list of non-value types
-- x App
-- x Pop
-- x Ite
-- x Cas
-- ? Let
-- x Ref
-- x Rec

-- The grand-ish list of value types
-- Num
-- Log
-- Con
-- Lam
-- Par

-- | A stepping evaluator.
-- More info needed here.
stepEval :: State -> State

--
-- Invalid states.
--

-- Unbound Name.
-- We can't do anything here. We're just putting this first so we
-- can fail early and fail explicitly. In the future, we could modify this
-- function to return an (Either State).
stepEval (State _ _ (Var v)) = error $ "error during eval: unbound variable " ++ v

--
-- Non-value Types.
--

-- Function application.
-- Returns either the function body or Par, depending on the number of arguments
-- provided. We need to make sure the expression has been evaluated to a lambda
-- or a partial evaluation first, though.
stepEval (State heap ctxs (App lam args)) = State heap (AppCtx args : ctxs) lam

-- Primitive operation.
-- We start with the left side first and when we're done, we move on to the right.
stepEval (State heap ctxs (Pop op lhs rhs)) = State heap (PopLtCtx op rhs : ctxs) lhs

-- If-Then-Else expression.
-- We evaluate the if-condition right now, storing the branches in a context
-- to pick later when the condition has been evaluated.
stepEval (State heap ctxs (Ite cond lt rt)) = State heap (IteCtx lt rt : ctxs) cond

-- Case expression.
-- We go through the alternatives one at a time. If they have guards, we need
-- to evaluate them too; we use a different Ctx for that.
--
--   0. Does the case have any alts? If no, we're stuck. Throw an error.
--   1. Does the first pattern match? If yes, move on. If no, delete that alt.
--   2. Does the first pattern have a guard? If no, we're done. If yes, move on.
--   3. Is that guard a value? If yes, great! Skip or return. If not, move on.
--   4. Pop the guard context onto the stack and continue evaluation next step.
stepEval (State heap ctxs (Cas mat (a:as))) = State heap (CasMatCtx a as : ctxs) mat

-- Let expression.
-- We don't support lazy pattern matching (~), so we have to fully evaluate
-- the let expression before we bind it.
--
-- Recursive lets are a problem. We need to evaluate the expression to
-- pattern-match, but the expressions could refer to the match variables.
-- The way we solve this problem is to use a fix directly if we see a PVar
-- or a PPat or use an explicit pattern match (with let) otherwise. Example:
--
-- > let a = ["hello"] ++ a
-- > let a = ["hello"] ++ ["hello"] ++ a
--
-- > let (a:_) = ["hello", "world"] ++ a
-- > let (a:_) = ["hello", "world"] ++ (let (a:_) = ["hello", "world"] ++ a in a)
--
-- Note that the binding available to the let clause and the in clause are
-- different, even though they refer to the same name.
--
-- 0. Do a regular inst across the let clauses, ignoring the recursive variables.
--    This would have been done before we even get to the let expression.
--
-- > let x = ["hello"] ++ (Var "x") ++ (Var "x") in ...
--
-- 1. Insert each let statement into the heap.
-- 2. Collect all the bindings from the let clause patterns and create an
--    association list of variable names to let-clauses.
--    We basically run find-and-replace.
--    i.e. [(String, Let [(Pat, Ref)] Var].
-- 3. Create a Fix for each one of those matches and stick em onto the heap.
--    i.e. [(String, Fix)]
-- 4. Use instFix, which replaces variables in an exp with Fixes.
--
-- > let x = ["hello"] ++ (Ref "x" 8) ++ (Ref "x" 8) in ...
-- > let x = ["hello"] ++ (Fix <binds> <let _f1 = ["hello"] ++ (Var "x") in _f1>) ++ ...
-- > let x = ["hello"] ++ (["hello"] ++ (Ref "x" 8) ++ (Ref "x" 8)) ++ ...
--
-- 5. When we reach a Fix, we insert each let statement into the heap.
--
-- > let x = ["hello"] ++ ["hello"] ++ (Ref "x" 9) in ...
--
-- When we're evaluating the Let, the only vars should be recursive ones, so
-- we shouldn't have issues where the Fix-ed binding is missing references.
--
-- Generally allocated as a Ref->Rec.
-- stepEval (State heap ctxs (Let binds expr)) =
--     instFix assocList expr
--   where
--     assocList = concatMap (uncurry fixBind) binds
--     fixBind pat bnd = map (\n -> matchVar n pat bnd) (AST.bindNames pat)

-- Heap reference.
-- We dereference it, of course. We also plop a RefCtx onto the stack so that we
-- can update the heap once this expression is properly evaluated. One exception
-- to this rule is if we're pulling out a Rec, since that's always fixed.
-- Not the best solution to this problem, but it's a solution :/
stepEval (State heap ctxs (Ref _ addr)) =
    State heap newCtxs exp
  where
    (_, exp) = Heap.deref addr heap
    newCtxs = if AST.isRec exp then ctxs else RefCtx addr : ctxs

-- Rec Instantiation.
-- Instantiate a Rec using variables from the stored scope.
stepEval (State heap ctxs (Rec vas exp)) = State heap ctxs (instExp vas exp)

-- Don't do anything if there's nothing to do.
stepEval s@(State _ [] exp) | AST.isValue exp = s

-- Just in case.
stepEval s = error $ "unexpected program state: " ++ show s
