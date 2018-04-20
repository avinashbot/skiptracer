-- | The Syntax used by the interpreter.
module Skiptracer.Syntax (
    Pat (..),
    Alt (..),
    Exp (..),

    -- Patterns
    bindings,

    -- Expressions
    isValue,
    isVar,
    isRef
) where

import           Control.Arrow (second, (***))

-- | A pattern used on the left-hand side of declarations and expressions.
data Pat
    = PCon String [Pat] -- ^ Constructor
    | PPat String Pat   -- ^ @-pattern
    | PNum Int          -- ^ Number
    | PLog Bool         -- ^ Boolean
    | PVar String       -- ^ Binding
    | PWld              -- ^ Wildcard
    deriving Show

-- | Fetch all created names from a Pat.
bindings :: Pat -> [String]
bindings (PVar b)    = [b]
bindings (PPat b p)  = b : bindings p
bindings (PCon b ps) = b : concatMap bindings ps
bindings _           = []

-- | A possibly guarded pattern matching expression.
data Alt = Alt Pat (Maybe Exp) Exp deriving Show

-- | A haskell expression.
data Exp

    -- | A number literal or a numerical value.
    = Num Int
    -- | A boolean literal or a boolean value.
    | Log Bool

    -- | A Datatype. Treated as a value by the interpreter, although it can
    -- be decomposed using pattern matching.
    | Con String [Exp]

    -- | Lambda Expression
    | Lam [Pat] Exp

    -- | Function application. The first argument must be a Lam or evaluate to
    -- a Lam.
    --
    -- Applications of multiple arguments are nested. As in:
    --   > App x [a, b] == App (App x a) b
    | App Exp [Exp]

    -- | If-Then-Else expression.
    | Ite Exp Exp Exp

    -- | Case Expression with Guarded Alts
    | Cas Exp [Alt]

    -- | Let Expression
    -- NOTE: Let *can* be guarded, but without pattern matching support, it's
    -- not really worth implementing.
    | Let [(Pat, Exp)] Exp

    -- | A named reference to an Exp on the heap.
    | Ref String Int

    -- | A shared Exp. When fully expanded, it should update the heap.
    | Shr Int Exp

    -- | A name of an unallocated variable.
    | Var String

    -- -- | Function with a multiple matches and guarded where clauses
    -- --
    -- -- Pattern matching is a great feature, but that can't be dealt with as
    -- -- a step-able or reduce-able expression, so it's out for now. A
    -- -- possible future workaround for this is "desugaring" them to a
    -- -- single function with a tuple-based case statement.
    -- --
    -- -- Furthermore, this makes "where" statements kinda redundant for now.
    -- --
    -- -- | Fun [(Grd [Pat], Exp, (Grd Pat, Exp))]

    deriving Show

-- | Is this expression a direct value?
isValue :: Exp -> Bool
isValue (Var v)   = error $ "unbound variable: " ++ v
isValue (Ref v _) = error $ "unresolved reference: " ++ v
isValue Con{}     = True
isValue Num{}     = True
isValue Log{}     = True
isValue Lam{}     = True
isValue _         = False

-- | Is this expression an unbound variable?
isVar :: Exp -> Bool
isVar (Var _) = True
isVar _       = False

-- | Is this expression a reference to an expression on the heap?
isRef :: Exp -> Bool
isRef (Ref _ _) = True
isRef _         = False
