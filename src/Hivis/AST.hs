-- | Contains high-level structures that represent an AST for a workable
-- Haskell subset.
module Hivis.AST (
    Exp (..),
    Pat (..),
    Dec (..),

    isValue,
    isVar,
    isRef,
    lookupDec
) where

-- | A lambda type indicator (mainly just )
data LamTyp = FuncLam | ExpLam deriving (Eq, Show)

-- | A haskell expression.
data Exp
    = Con String [Exp]        -- ^ Constructor call
    | Num Int                 -- ^ Numerical value
    | Log Bool                -- ^ Boolean value
    | Var String              -- ^ A name of an unallocated variable
    | Pop String Exp Exp      -- ^ Primitive operation
    | App String [Exp]        -- ^ Function application with arguments
    | Ite Exp Exp Exp         -- ^ If-Then-Else expression
    | Cas Exp [(Pat, Exp)]    -- ^ A case expression with a bunch of alts

    -- Only available to initialized ASTs
    | Ref String Int          -- ^ A named reference to an exp on the heap

    -- Not implemented.
    -- Functions are raw lambdas holding the global scope.
    -- We can finally shove em onto the heap now!
    | Raw [(String, Int)] Exp    -- ^ Uninitialized expression (with scope)
    | Let [(Pat, Exp)] Exp       -- ^ Let expression
    | Lam [Pat] Exp [(Pat, Exp)] -- ^ Lambda expression (with where bindings)
    deriving Show

-- | Is this expression a direct value?
isValue :: Exp -> Bool
isValue (Var v)   = error $ "unbound variable: " ++ v
isValue (Ref v _) = error $ "unresolved reference: " ++ v
isValue Raw{}     = error "uninitialized expression"
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

-- | A pattern used on the left-hand side of declarations and expressions.
data Pat = PCon String [Pat]
         | PNum Int    -- ^ Number
         | PLog Bool   -- ^ Boolean
         | PVar String -- ^ Binding
         | PWld        -- ^ Wildcard
         deriving Show

-- | Programs are represented as lists of named function declarations,
-- where each declaration is a value of the following datatype.
-- TODO: maybe change the body to be list of guard-expression pairs?
data Dec = Dec { name   :: String -- ^ The function name
               , params :: [Pat]  -- ^ The function arguments
               , body   :: Exp    -- ^ The function body
               } deriving Show

-- | Lookup a declaration by name, erroring if one wasn't found.
lookupDec :: String -> [Dec] -> Dec
lookupDec f []     = error $ "undeclared function: " ++ f
lookupDec f (d:ds) = if f == name d then d else lookupDec f ds
