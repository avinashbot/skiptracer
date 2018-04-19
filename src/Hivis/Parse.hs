module Hivis.Parse where

import           Hivis.AST             (Dec (..), Exp (..), Pat (..))
import qualified Language.Haskell.Exts as Hs

class ToExp a where toExp :: a -> Exp
class ToPat a where toPat :: a -> Pat
class ToDec a where toDec :: a -> Dec

instance ToExp (Hs.Exp l) where
    toExp (Hs.Paren _ e)            = toExp e
    toExp (Hs.Lit _ (Hs.Int _ i _)) = Num (fromIntegral i)
    toExp (Hs.List _ [])            = Con "[]" []

    toExp (Hs.If _ cd lt rt)        = Ite (toExp cd) (toExp lt) (toExp rt)

    toExp _                         = undefined

instance ToPat (Hs.Pat l) where
    toPat (Hs.PParen _ p)             = toPat p
    toPat (Hs.PWildCard _)            = PWld
    toPat (Hs.PVar _ (Hs.Ident _ s))  = PVar s
    toPat (Hs.PVar _ (Hs.Symbol _ s)) = PVar s
    toPat _                           = undefined
