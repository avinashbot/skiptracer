-- | The AST used by the interpreter.
module Skiptracer1.AST (
    Pat (..),
    Grd (..),
    Exp (..),

    bindNames,
    isValue,
    isVar,
    isRef,
    isRec,
    arity
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

-- | A possibly guarded pattern matching expression.
data Grd a = Grd a (Maybe Exp) deriving Show

-- | A haskell expression.
data Exp
    -- * Primitives
    -- TODO: a tuple primitive is probably a good idea

    -- | A number literal or a numerical value.
    = Num Int
    -- | A boolean literal or a boolean value.
    | Log Bool

    -- * Compound Expressions

    -- | A Datatype. Treated as a value by the interpreter, although it can
    -- be decomposed using pattern matching.
    | Con String [Exp]

    -- | Lambda Expression
    | Lam [Pat] Exp

    -- | Function application. The first argument is a Lam, Par or a Ref->Rec.
    -- This isn't expanded until the number of parameters is equal to the
    -- function arity.
    | App Exp [Exp]

    -- | A primitive operation (e.g. "*", "==").
    | Pop String Exp Exp

    -- | If-Then-Else expression.
    | Ite Exp Exp Exp

    -- | Case Expression with Guarded Alts
    | Cas Exp [(Grd Pat, Exp)]

    -- | Let Expression
    -- NOTE: Let *can* be guarded, but without pattern matching support, it's
    -- not really worth it.
    | Let [(Pat, Exp)] Exp

    -- -- | Function with a single and guarded where clauses
    -- --
    -- -- Pattern matching is a great feature, but that can't be dealt with as
    -- -- a step-able or reduce-able expression, so it's out for now. A
    -- -- possible future workaround for this is "desugaring" them to a
    -- -- single function with a tuple-based case statement.
    -- --
    -- -- Furthermore, this makes "where" statements kinda redundant for now.
    -- --
    -- -- | Fun [] Exp [(Grd Pat, Exp)]

    -- * Eval-Time Expressions

    -- | A named reference to an Exp on the heap.
    | Ref String Int

    -- | Partial function application. This is a value (and so is the first Exp).
    | Par Exp [Exp]

    -- * Pre-Eval-Time Expressions

    -- | A name of an unallocated variable
    | Var String

    -- | A special-case Ref, used for recursive bindings.
    -- Basically, it instantiates the expression before using it. This is meant
    -- for any named variable that can recurse (i.e. recursive let and
    -- top-level functions)
    --
    -- Stored on the heap as a Ref->Rec but treated specially because this heap reference
    -- shouldn't actually resolve to a value. There might be a less clumsy way to
    -- deal with this...
    --
    -- NOTE: Ask Colin for a possible alternative that doesn't suck
    -- NOTE: remember when making a Rec to a Let/Fun to not track the inner scope.
    -- NOTE: a possible space optimization could be to only keep track of variables that we know will be used?
    | Rec [(String, Int)] Exp

    -- | For recursion.
    -- More details needed here.
    | Fix [(String, Exp)] Exp

    deriving Show

-- -- | Update each node in the Exp tree.
-- update :: (Exp -> Exp) -> Exp -> Exp
-- update f e =
--     case f e of
--         (Con s es)     -> Con s (map (update f) es)
--         (Lam ps ex)    -> Lam ps (update f ex)
--         (App l es)     -> App (update f l) (map (update f) es)
--         (Pop s e1 e2)  -> Pop s (update f e1) (update f e2)
--         (Ite ec et ef) -> Ite (update f ec) (update f et) (update f ef)
--         (Cas ec al)    -> Cas (update f ec) (map (updateGrd *** update f) al)
--         (Let ps ex)    -> Let (map (second (update f)) ps) (update f ex)
--         val            -> val
--   where
--     updateGrd (Grd a me) = Grd a (fmap (update f) me)

-- | Fetch all created names from a Pat.
bindNames :: Pat -> [String]
bindNames (PVar b)    = [b]
bindNames (PPat b p)  = b : bindNames p
bindNames (PCon b ps) = b : concatMap bindNames ps
bindNames _           = []

-- | Is this expression a direct value?
isValue :: Exp -> Bool
isValue (Var v)   = error $ "unbound variable: " ++ v
isValue (Ref v _) = error $ "unresolved reference: " ++ v
isValue Con{}     = True
isValue Num{}     = True
isValue Log{}     = True
isValue Lam{}     = True
isValue Par{}     = True
isValue _         = False

-- | Is this expression an unbound variable?
isVar :: Exp -> Bool
isVar (Var _) = True
isVar _       = False

-- | Is this expression a reference to an expression on the heap?
isRef :: Exp -> Bool
isRef (Ref _ _) = True
isRef _         = False

-- | Is this expression a reference to the parse tree?
isRec :: Exp -> Bool
isRec Rec{} = True
isRec _     = False

-- | Lambda arity guess.
-- Used to figure out when to evaluate and when to curry.
arity :: Exp -> Int
arity (Lam a _) = length a
arity _         = error "arity only valid for lambdas/functions"
