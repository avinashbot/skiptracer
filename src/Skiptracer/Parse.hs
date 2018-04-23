module Skiptracer.Parse (
    parse,
    parseSimpleGhc,
    parseGhc
) where

import           Data.Maybe                   (mapMaybe)
import qualified Language.Haskell.Exts        as Hs
import qualified Language.Haskell.Exts.Simple as Shs
import           Skiptracer.Syntax            (Alt (..), Exp (..), Pat (..))

parse :: String -> Exp
parse = toExp . parseGhc

parseGhc :: String -> Hs.Module Hs.SrcSpanInfo
parseGhc src = case Hs.parseModule src of
    Hs.ParseOk a       -> a
    Hs.ParseFailed a s -> error ("ERROR: " ++ s)

parseSimpleGhc :: String -> Shs.Module
parseSimpleGhc src = case Shs.parseModule src of
    Hs.ParseOk a       -> a
    Hs.ParseFailed a s -> error ("ERROR: " ++ s)

class ToExp a where toExp :: a -> Exp

instance ToExp (Hs.Module l) where
    toExp (Hs.Module _ _ _ _ ds) = Let (mapMaybe toDec ds) (Var "main")

instance ToExp (Hs.Exp l) where
    toExp (Hs.Paren _ e) = toExp e

    -- Num
    toExp (Hs.Lit _ (Hs.Int _ i _))               = Num (fromIntegral i)
    toExp (Hs.NegApp _ (Hs.Lit _ (Hs.Int _ i _))) = Num (negate (fromIntegral i))

    -- Log
    toExp (Hs.Con _ (Hs.UnQual _ (Hs.Ident _ "True"))) = Log True
    toExp (Hs.Con _ (Hs.UnQual _ (Hs.Ident _ "False"))) = Log False

    -- Con "[]"
    toExp (Hs.List _ [])     = Con "[]" []
    toExp (Hs.List l (e:es)) = Con ":" [toExp e, toExp (Hs.List l es)]

    -- Con "(,)"
    toExp (Hs.Tuple _ _ es) =
        Con (replicate (length es) ',') (map toExp es)

    -- Lam
    toExp (Hs.Lambda _ ps e) = Lam Nothing (map toPat ps) (toExp e)

    -- App
    toExp (Hs.InfixApp _ e1 op e2) = App (toExp op) [toExp e1, toExp e2]
    toExp (Hs.App _ f1 a1) =
        case toExp f1 of
            App (Var n) a2 -> App (Var n) (a2 ++ [toExp a1])
            f2             -> App f2 [toExp a1]
    toExp (Hs.LeftSection _ e o) =
        Lam Nothing [PVar "x"] (App (toExp o) [Var "x", toExp e])
    toExp (Hs.RightSection _ o e) =
        Lam Nothing [PVar "x"] (App (toExp o) [toExp e, Var "x"])

    -- Ite
    toExp (Hs.If _ cd lt rt) = Ite (toExp cd) (toExp lt) (toExp rt)

    -- Cas
    toExp (Hs.Case _ e as) = Cas (toExp e) (concatMap toAlt as)

    -- Let
    toExp (Hs.Let _ (Hs.BDecls _ bs) e) = Let (mapMaybe toDec bs) (toExp e)

    -- Var
    toExp (Hs.Var _ (Hs.UnQual _ n)) = Var (name n)


instance ToExp (Hs.QOp l) where
    toExp (Hs.QVarOp _ (Hs.UnQual _ n))               = Var (name n)
    toExp (Hs.QConOp _ (Hs.UnQual _ n))               = Con (name n) []

    toExp (Hs.QConOp _ (Hs.Special _ (Hs.UnitCon _))) = Con "()" []
    toExp (Hs.QConOp _ (Hs.Special _ (Hs.ListCon _))) = Con "[]" []
    toExp (Hs.QConOp _ (Hs.Special _ (Hs.Cons _)))    = Con ":" []
    toExp (Hs.QConOp _ (Hs.Special _ (Hs.TupleCon _ _ a))) =
        Con (replicate a ',') []


instance ToExp (Hs.Op l) where
    toExp (Hs.VarOp _ n) = Var (name n)
    toExp (Hs.ConOp _ n) = Con (name n) []


name :: Hs.Name l -> String
name (Hs.Ident _ s)  = s
name (Hs.Symbol _ s) = s

toPat :: Hs.Pat l -> Pat
toPat (Hs.PParen _ p)             = toPat p
toPat (Hs.PWildCard _)            = PWld
toPat (Hs.PVar _ (Hs.Ident _ s))  = PVar s
toPat (Hs.PVar _ (Hs.Symbol _ s)) = PVar s
toPat (Hs.PLit _ (Hs.Signless _) (Hs.Int _ i _)) = PNum (fromIntegral i)
toPat (Hs.PLit _ (Hs.Negative _) (Hs.Int _ i _)) = PNum (negate (fromIntegral i))
toPat (Hs.PAsPat _ n p) = PPat (name n) (toPat p)
toPat (Hs.PInfixApp _ p1 (Hs.Special _ (Hs.Cons _)) p2) = PCon ":" [toPat p1, toPat p2]
toPat (Hs.PApp _ (Hs.UnQual _ (Hs.Ident _ "True")) []) = PLog True
toPat (Hs.PApp _ (Hs.UnQual _ (Hs.Ident _ "False")) []) = PLog False
toPat (Hs.PApp _ (Hs.Special _ (Hs.ListCon _)) ps) = PCon "[]" (map toPat ps)
toPat (Hs.PApp _ (Hs.Special _ (Hs.Cons _)) ps)    = PCon ":" (map toPat ps)
toPat (Hs.PApp _ (Hs.Special _ (Hs.TupleCon _ _ a)) ps) =
    PCon ("(" ++ replicate a ',' ++ ")") (map toPat ps)
toPat (Hs.PTuple _ _ ps) =
    PCon ("(" ++ replicate (length ps) ',' ++ ")") (map toPat ps)
toPat (Hs.PList _ [])     = PCon "[]" []
toPat (Hs.PList l (p:ps)) = PCon ":" [toPat p, toPat (Hs.PList l ps)]

toAlt :: Hs.Alt l -> [Alt]
toAlt (Hs.Alt _ p (Hs.UnGuardedRhs _ e) Nothing) = [Alt (toPat p) Nothing (toExp e)]
toAlt (Hs.Alt _ p (Hs.GuardedRhss _ rs) Nothing) = map toGuardedAlt rs
  where
    toGuardedAlt (Hs.GuardedRhs _ [Hs.Qualifier _ g] e) =
        Alt (toPat p) (Just (toExp g)) (toExp e)

toDec :: Hs.Decl l -> Maybe (Pat, Exp)
toDec (Hs.FunBind _ [Hs.Match _ n ps (Hs.UnGuardedRhs _ e) Nothing]) =
    Just (PVar (name n), Lam (Just (name n)) (map toPat ps) (toExp e))
toDec (Hs.PatBind _ p (Hs.UnGuardedRhs _ e) Nothing) =
    Just (toPat p, toExp e)
toDec _ = Nothing
