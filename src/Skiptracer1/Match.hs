module Skiptracer1.Match where

import           Control.Monad   (liftM2)
import           Data.Maybe      (fromMaybe)
import           Skiptracer1.AST (Exp (..), Pat (..))

-- | matchOne checks if an expression matches a pattern.
-- It also returns a list of bindings resulting from the match.
matchOne :: Exp -> Pat -> Maybe [(String, Exp)]
matchOne _          PWld        = Just []
matchOne e          (PVar b)    = Just [(b, e)]
matchOne e          (PPat b p)  = Just $ (b, e) : fromMaybe [] (matchOne e p)
matchOne (Num a)    (PNum b)    | a == b = Just []
matchOne (Log a)    (PLog b)    | a == b = Just []
matchOne (Con a xs) (PCon b ys) | a == b = matchAll xs ys
matchOne (Var v)    _           = error $ "cannot match against unbound variable: " ++ v
matchOne (Ref v _)  _           = error $ "cannot match against unresolved reference: " ++ v
matchOne _          _           = Nothing

-- | matchAll works similar to matchOne, but checks for corresponding matches
-- over lists of exps and pats. Used for functions/lambdas.
matchAll :: [Exp] -> [Pat] -> Maybe [(String, Exp)]
matchAll [] []         = Just []
matchAll _  []         = Nothing
matchAll [] _          = Nothing
matchAll (x:xs) (y:ys) = liftM2 (++) (matchOne x y) (matchAll xs ys)
