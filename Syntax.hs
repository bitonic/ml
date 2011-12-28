module Syntax
       ( Id
       , Decl (..)
       , DataBody
       , Type (..)
       , Var (..)
       , var
       , Con (..)
       , con
       , Literal (..)
       , Pattern (..)
       , Term (..)
         -- * Instances
       , FullTerm
       , DTerm
         -- * Utils
       , tupleCon
       , tupleType
       , tupleTerm
         -- * Pretty printing
       ) where

-------------------------------------------------------------------------------

type Id = String

data Decl t = ValDecl Var t
            | TypeSig Var Type
            | DataDecl Con [Var] DataBody
            deriving (Show, Eq)

newtype Var = VarN {unVar :: Id}
            deriving (Eq, Ord)
newtype Con = ConN {unCon :: Id}
            deriving (Eq, Ord)

instance Show Var where
    show = show . unVar

instance Show Con where
    show = show . unCon

var :: Id -> Var
var = VarN

con :: Id -> Con
con = ConN

data Type = TyCon Con
          | TyApp Type Type
          | TyVar Var
          | TyGen Int
          deriving (Show, Eq)

data Literal = IntLit String
             | RealLit String
             deriving (Show, Eq)

data Pattern = VarPat Var
             | Pat Con [Pattern]
             | IntPat String
             deriving (Show, Eq)

data Term fn lt = Var Var
                | Con Con
                | Abs fn (Term fn lt)
                | App (Term fn lt) (Term fn lt)
                | Let lt (Term fn lt) (Term fn lt)
                | Literal Literal
                | Case (Term fn lt) [(Pattern, (Term fn lt))]
                deriving (Show, Eq)

type DataBody = [(Con, [Type])]

-------------------------------------------------------------------------------

type FullTerm = Term [Pattern] Pattern

type DTerm    = Term Var Var

-------------------------------------------------------------------------------

tupleCon :: Int -> Con
tupleCon n = con $ "(" ++ replicate (n - 1) ','  ++ ")"

tupleType :: [Type] -> Type
tupleType tss = foldl TyApp (TyCon (tupleCon (length tss))) tss

tupleTerm :: [Term fn lt] -> Term fn lt
tupleTerm ts = foldl App (Con (tupleCon (length ts))) ts
