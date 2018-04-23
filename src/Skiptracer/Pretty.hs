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

import           Data.Char             (isSymbol, isUpper)
import qualified Language.Haskell.Exts as Hs
import           Skiptracer.Syntax     (Alt (..), Exp (..), Pat (..))
import qualified Skiptracer.Syntax     as Syntax

prettyPrint :: Exp -> String
prettyPrint = Hs.prettyPrint . expr

expr :: Exp -> Hs.Exp Hs.SrcSpanInfo
expr (Pop n)                 = Hs.Var l (qname n)
expr (Var n)                 = Hs.Var l (qname n)
expr (Num n)                 | n >= 0 = Hs.Lit l (Hs.Int l (fromIntegral n) (show n))
                             | n < 0  = Hs.NegApp l (Hs.Lit l (Hs.Int l (fromIntegral (negate n)) (show n)))
expr (Log True)              = Hs.Con l (qname "True")
expr (Log False)             = Hs.Con l (qname "False")
expr (Con n [])              = Hs.Con l (qname n)
expr (Con n es)              = expr (App (Con n []) es)
expr (Lam (Just n) _ _)      = Hs.Var l (qname n)
expr (Lam Nothing ps e)      = Hs.Lambda l (map pat ps) (expr e)
expr (App (Pop x) [a, b])    = Hs.InfixApp l (expr a) (qop x) (expr b)
expr (App (Con x []) [a, b]) | x == ":" = Hs.InfixApp l (expr a) (qop x) (expr b)
expr (App a bs)              = foldl (Hs.App l) (expr a) (map expr bs)
expr (Ite c t f)             = Hs.If l (expr c) (expr t) (expr f)
expr (Cas e as)              = Hs.Case l (expr e) (map alt as)
expr (Let ms e)              = Hs.Let l (Hs.BDecls l (map decl ms)) (expr e)
expr (Ref n i)               = Hs.Var l (qname (n ++ show i))

alt :: Alt -> Hs.Alt Hs.SrcSpanInfo
alt (Alt p Nothing e)  = Hs.Alt l (pat p) (Hs.UnGuardedRhs l (expr e)) Nothing
alt (Alt p (Just g) e) = Hs.Alt l (pat p) (Hs.GuardedRhss l [Hs.GuardedRhs l [Hs.Qualifier l (expr g)] (expr e)]) Nothing

decl :: (Pat, Exp) -> Hs.Decl Hs.SrcSpanInfo
decl (p, Lam (Just n) ps e) = Hs.FunBind l [Hs.Match l (name n) (map pat ps) (Hs.UnGuardedRhs l (expr e)) Nothing]
decl (p, e)                 = Hs.PatBind l (pat p) (Hs.UnGuardedRhs l (expr e)) Nothing

pat :: Pat -> Hs.Pat Hs.SrcSpanInfo
pat (PCon ":" [p, q]) = Hs.PInfixApp l (pat p) (qname ":") (pat q)
pat (PCon n ps)       = Hs.PApp l (qname n) (map pat ps)
pat (PPat n p)        = Hs.PAsPat l (name n) (pat p)
pat (PNum i)          = Hs.PLit l (if i < 0 then Hs.Negative l else Hs.Signless l) (Hs.Int l (fromIntegral i) (show i))
pat (PLog True)       = Hs.PApp l (qname "True") []
pat (PLog False)      = Hs.PApp l (qname "False") []
pat (PVar n)          = Hs.PVar l (name n)
pat PWld              = Hs.PWildCard l

qop :: String -> Hs.QOp Hs.SrcSpanInfo
qop n = if isUpper (head n) then Hs.QConOp l (qname n) else Hs.QVarOp l (qname n)

qname :: String -> Hs.QName Hs.SrcSpanInfo
qname "()" = Hs.Special l (Hs.UnitCon l)
qname "[]" = Hs.Special l (Hs.ListCon l)
qname ":"  = Hs.Special l (Hs.Cons l)
qname n
    | all (== ',') n = Hs.Special l (Hs.TupleCon l Hs.Boxed (length n))
    | otherwise      = Hs.UnQual l (name n)

name :: String -> Hs.Name Hs.SrcSpanInfo
name n
    | all isSymbol n = Hs.Symbol l n
    | otherwise      = Hs.Ident l n

l :: Hs.SrcSpanInfo
l = Hs.noSrcSpan
