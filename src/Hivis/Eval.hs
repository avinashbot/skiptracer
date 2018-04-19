-- | Contains a stepping evaluator as well as a couple of convenience
-- functions.
module Hivis.Eval (
    Ctx   (..),
    State (..),

    fromExp,
    isFinal,

    eval,
    evalTrace,
    evalSimple
) where

import           Control.Arrow (second)
import           Control.Monad (liftM2)
import           Data.List     (concatMap)
import           Data.Maybe    (fromJust, isJust, maybe)

import           Hivis.AST     (Dec, Exp (..), Pat (..))
import qualified Hivis.AST     as AST
import           Hivis.Heap    (Heap)
import qualified Hivis.Heap    as Heap

-- | Context object. Contexts are meant to represent language-level things
-- like ifs, cases and primitive operations.
data Ctx = IteCtx Exp Exp      -- ^ If-Then-Else with the two branches
         | CasCtx [(Pat, Exp)] -- ^ Case with remaining alts
         | RefCtx Int          -- ^ Variable reference "tracker" with address
         | PopLtCtx String Exp -- ^ First argument of primitive operation
         | PopRtCtx String Exp -- ^ Final argument of primitive operation
         deriving (Show)

-- | Data constructor for the interpreter state.
data State = State [Ctx] (Heap Exp) Exp deriving Show

-- | Create a "blank" state from an expression.
fromExp :: Exp -> State
fromExp = State [] Heap.empty

-- | Returns whether a state is the final state of the program.
isFinal :: State -> Bool
isFinal (State ctxs _ exp) = null ctxs && AST.isValue exp

-- | Evaluate a primitive operation.
primOp :: String -> Exp -> Exp -> Exp
primOp "+"  (Num m) (Num n) = Num (m + n)
primOp "-"  (Num m) (Num n) = Num (m - n)
primOp "*"  (Num m) (Num n) = Num (m * n)
primOp "==" (Num m) (Num n) = Log (m == n)
primOp "<=" (Num m) (Num n) = Log (m <= n)
primOp s    e1      e2      = error $ "invalid primOp: " ++ unwords [show e1, s, show e2]

-- | The inst function constructs a fresh instance of a function body
-- given an association-list binding argument names to the addresses
-- where corresponding arguments are stored in a heap.
inst :: [(String, Int)] -> Exp -> Exp
inst vas (Var v)        = maybe (Var v) (Ref v) (lookup v vas)
inst vas (Pop s e1 e2)  = Pop s (inst vas e1) (inst vas e2)
inst vas (Con s es)     = Con s (map (inst vas) es)
inst vas (App f es)     = App f (map (inst vas) es)
inst vas (Ite ec et ee) = Ite (inst vas ec) (inst vas et) (inst vas ee)
inst vas (Cas ec al)    = Cas (inst vas ec) (map (second (inst vas)) al)
inst _   e              = e

-- | matchOne checks if an expression matches a pattern.
-- It also returns a list of bindings resulting from the match.
matchOne :: Exp -> Pat -> Maybe [(String, Exp)]
matchOne _          PWld        = Just []
matchOne e          (PVar b)    = Just [(b, e)]
matchOne (Num a)    (PNum b)    | a == b = Just []
matchOne (Log a)    (PLog b)    | a == b = Just []
matchOne (Con a xs) (PCon b ys) | a == b = matchMany xs ys
matchOne _          _           = Nothing

-- | matchMany works similar to matchOne, but checks for corresponding matches
-- over lists of exps and pats.
matchMany :: [Exp] -> [Pat] -> Maybe [(String, Exp)]
matchMany [] []         = Just []
matchMany _  []         = Nothing
matchMany [] _          = Nothing
matchMany (x:xs) (y:ys) = liftM2 (++) (matchOne x y) (matchMany xs ys)

-- | A stepping evaluator.
-- Takes a list of declarations and the current state and returns the next
-- state of the program.
-- NOTE: This might benefit from splitting into "non-value" and "value" functions.
eval :: [Dec] -> State -> State

-- Function expansion/evaluation (all contexts need to expand this)
-- NOTE: fromJust isn't the greatest long-term idea
eval decs (State ctxs heap (App name args)) =
    State ctxs heap1 fninst
  where
    func           = AST.lookupDec name decs
    binds          = fromJust $ matchMany args (AST.params func)
    (addrs, heap1) = Heap.allocMany (map snd binds) heap
    fninst         = inst (zip (map fst binds) addrs) (AST.body func)

-- Variable dereferencing (in the expression)
-- We basically tell the context to remember that the following expression
-- needs to be updated when we get a value out of the resulting expression.
eval decs (State ctxs heap (Ref _ addr)) =
    State (RefCtx addr : ctxs) heap (snd (Heap.deref addr heap))

-- Primitive operation
eval decs (State ctxs heap (Pop op lhs rhs)) =
    State (PopLtCtx op rhs : ctxs) heap lhs

-- If-Then-Else condition
eval decs (State ctxs heap (Ite cond ltExp rtExp)) =
    State (IteCtx ltExp rtExp : ctxs) heap cond

-- Case Expression
eval decs (State ctxs heap (Cas exp alts)) =
    State (CasCtx alts : ctxs) heap exp

-- Update Ref with resulting value
-- NOTE: This must appear before the other value steps!
-- We don't need to check for isRef (because it's already handled)
-- We don't need to check for isVar (because that shouldn't happen)
eval decs (State (RefCtx addr : ctxs) heap exp) | AST.isValue exp =
    State ctxs (Heap.update addr exp heap) exp

-- First argument of primitive function
eval decs (State (PopLtCtx op rhs : ctxs) heap lhs) | AST.isValue lhs =
    State (PopRtCtx op lhs : ctxs) heap rhs

-- Second argument of primitive function
eval decs (State (PopRtCtx op lhs : ctxs) heap rhs) | AST.isValue rhs =
    State ctxs heap (primOp op lhs rhs)

-- If-Then-Else condition evaluation
eval decs (State (IteCtx ltExp rtExp : ctxs) heap (Log cond)) =
    State ctxs heap branch
  where
    branch = if cond then ltExp else rtExp

-- Case expression
eval decs (State (CasCtx ((pat, body) : alts) : ctxs) heap exp)
    | AST.isValue exp && isJust binds = State ctxs heap1 altinst
    | AST.isValue exp && null alts    = error "exhausted all patterns in case"
    | AST.isValue exp                 = State (CasCtx alts : ctxs) heap exp
  where
    binds          = matchOne exp pat
    (addrs, heap1) = Heap.allocMany (map snd (fromJust binds)) heap
    altinst        = inst (zip (map fst (fromJust binds)) addrs) body

-- Don't do anything if there's nothing to do.
eval decs s@(State [] _ exp) | AST.isValue exp = s

-- Just your usual defensive programming in case I missed something.
eval _ s = error $ "unexpected program state: " ++ show s

-- Run eval, preserving the program steps.
evalTrace :: [Dec] -> State -> [State]
evalTrace decs state
    | isFinal state = [state]
    | otherwise     = state : evalTrace decs (eval decs state)

-- Evaluate a basic expression without any setup.
evalSimple :: Exp -> Exp
evalSimple start = final
  where
    (State _ _ final) = until isFinal (eval []) (fromExp start)
