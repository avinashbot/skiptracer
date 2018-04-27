module Skiptracer.Eval.Match (
    matchable,
    match
) where

import           Control.Monad     (liftM2)
import           Data.Maybe        (fromMaybe)
import           Skiptracer.Syntax (Exp (..), Pat (..))
import qualified Skiptracer.Syntax as Syntax

-- | Returns whether an expression is sufficiently evaluated to a point where
-- match can be called.
--
-- It's important that matchable returns False for "terminal" expressions.
matchable :: Exp -> Pat -> Bool
matchable _          PWld        = True
matchable _          (PVar _)    = True
matchable e          (PPat _ p)  = matchable e p
matchable (Con n es) (PCon m ps) = n /= m || all (uncurry matchable) (zip es ps)
matchable e          _           = Syntax.isValue e

-- | match checks if an expression matches a pattern.
-- It also returns a list of bindings resulting from the match.
match :: Exp -> Pat -> Maybe [(String, Exp)]
match _          PWld        = Just []
match e          (PVar b)    = Just [(b, e)]
match e          (PPat b p)  = Just $ (b, e) : fromMaybe [] (match e p)
match (Num a)    (PNum b)    | a == b = Just []
match (Chr a)    (PChr b)    | a == b = Just []
match (Log a)    (PLog b)    | a == b = Just []
match (Con a xs) (PCon b ys) | a == b = matchAll xs ys
match (Var v)    _           = error $ "cannot match against unbound variable: " ++ v
match (Ref v _)  _           = error $ "cannot match against unresolved reference: " ++ v
match _          _           = Nothing

-- | matchAll works similar to match, but checks for corresponding matches
-- over lists of exps and pats. Used for functions/lambdas.
matchAll :: [Exp] -> [Pat] -> Maybe [(String, Exp)]
matchAll [] []         = Just []
matchAll _  []         = Nothing
matchAll [] _          = Nothing
matchAll (x:xs) (y:ys) = liftM2 (++) (match x y) (matchAll xs ys)
