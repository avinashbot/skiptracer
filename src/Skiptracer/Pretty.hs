-- | Pretty prints an Exp as a String.
--
-- Currently translates to haskell-src for pretty printing, but a more
-- closely-integrated parsing/printing mechanism may be better.
--
-- On the other hand, Haskell comes with weird intentation rules that are
-- best left to more experienced developers.
module Skiptracer.Pretty (
    prettyPrint
) where

import           Data.Char               (isSymbol)
import qualified Language.Haskell.Pretty as PP
import           Language.Haskell.Syntax
import           Skiptracer.Syntax       (Alt (..), Exp (..), Pat (..))

prettyPrint :: Exp -> String
prettyPrint = PP.prettyPrint . expr

noSrcLoc :: SrcLoc
noSrcLoc = SrcLoc "" 0 0

expr :: Exp -> HsExp
expr (Var n)            = HsVar (qname n)
expr (Num n)            | n >= 0 = HsLit (HsInt (fromIntegral n))
                        | n < 0  = HsNegApp (HsLit (HsInt (fromIntegral (negate n))))
expr (Log True)         = HsCon (qname "True")
expr (Log False)        = HsCon (qname "False")
expr (Con n [])         = HsCon (qname n)
expr (Con n es)         = expr (App (Con n []) es)
expr (Lam (Just n) _ _) = HsVar (qname n)
expr (Lam Nothing ps e) = HsLambda noSrcLoc (map pat ps) (expr e)
expr (App a bs)         = foldl HsApp (expr a) (map expr bs)
expr (Ite c t f)        = HsIf (expr c) (expr t) (expr f)
expr (Cas e as)         = HsCase (expr e) (map alt as)
expr (Let ms e)         = HsLet (map decl ms) (expr e)
expr (Ref n i)          = HsVar (qname (n ++ show i))

alt :: Alt -> HsAlt
alt (Alt p Nothing e)  = HsAlt noSrcLoc (pat p) (HsUnGuardedAlt (expr e)) []
alt (Alt p (Just g) e) = HsAlt noSrcLoc (pat p) (HsGuardedAlts [HsGuardedAlt noSrcLoc (expr g) (expr e)]) []

decl :: (Pat, Exp) -> HsDecl
decl (p, Lam (Just n) ps e) = HsFunBind [HsMatch noSrcLoc (name n) (map pat ps) (HsUnGuardedRhs (expr e)) []]
decl (p, e)                 = HsPatBind noSrcLoc (pat p) (HsUnGuardedRhs (expr e)) []

pat :: Pat -> HsPat
pat (PCon n ps)  = HsPApp (qname n) (map pat ps)
pat (PPat n p)   = HsPAsPat (name n) (pat p)
pat (PNum i)     = HsPLit (HsInt (fromIntegral i))
pat (PLog True)  = HsPApp (qname "True") []
pat (PLog False) = HsPApp (qname "False") []
pat (PVar n)     = HsPVar (name n)
pat PWld         = HsPWildCard

qname :: String -> HsQName
qname "()" = Special HsUnitCon
qname "[]" = Special HsListCon
qname ":"  = Special HsCons
qname n
    | all (== ',') n = Special (HsTupleCon (length n))
    | otherwise      = UnQual (name n)

name :: String -> HsName
name n
    | all isSymbol n = HsSymbol n
    | otherwise      = HsIdent n
