module Skiptracer.Eval.Bind (bind) where

import           Control.Arrow     (second)
import           Skiptracer.Syntax (Alt (..), Exp (..), Pat (..))
import qualified Skiptracer.Syntax as Syntax

-- | Allocate a set of expressions, taking into account variable shadowing.
bind :: [String] -> [(String, Int)] -> Exp -> Exp
bind sh ad (Con n e)   = Con n (map (bind sh ad) e)
bind sh ad (Lam p e)   = Lam p (bind (concatMap Syntax.bindings p ++ sh) ad e)
bind sh ad (App f r)   = App (bind sh ad f) (map (bind sh ad) r)
bind sh ad (Ite c l r) = Ite (bind sh ad c) (bind sh ad l) (bind sh ad r)
bind sh ad (Cas c as) =
    Cas (bind sh ad c) (map bindAlt as)
  where
    bindAlt (Alt p g e) =
        let sh1 = Syntax.bindings p ++ sh
        in  Alt p (fmap (bind sh1 ad) g) (bind sh1 ad e)
bind sh ad (Let b e) =
    Let (map (second (bind sh1 ad)) b) (bind sh1 ad e)
  where
    sh1 = concatMap (Syntax.bindings . fst) b ++ sh
bind sh ad e@(Var n)
    | n `elem` sh = e
    | otherwise   = maybe e (Ref n) (lookup n ad)
bind _ _ e = e
