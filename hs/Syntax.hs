module Syntax
       ( Id
       , Decl (..)
       , DataBody
       , Type (..)
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

data Decl t = ValDecl Id t
            | DataDecl Id [Id] DataBody
            deriving (Show, Eq)

data Type = TyCon Id
          | TyApp Type Type
          | TyVar Id
          | TyGen Int
          deriving (Show, Eq)

data Literal = IntLit Id
             | RealLit Id
             deriving (Show, Eq)

data Pattern = VarPat Id
             | Pat Id [Pattern]
             | LitPat Literal
             deriving (Show, Eq)

data Term fn lt = Var Id
                | Con Id
                | Abs fn (Term fn lt)
                | App (Term fn lt) (Term fn lt)
                | Let lt (Term fn lt) (Term fn lt)
                | Literal Literal
                | Case (Term fn lt) [(Pattern, (Term fn lt))]
                deriving (Show, Eq)

type DataBody = [(Id, [Type])]

-------------------------------------------------------------------------------

type FullTerm = Term [Pattern] Pattern

type DTerm    = Term Id Id

-------------------------------------------------------------------------------

tupleCon :: Int -> String
tupleCon n = "(" ++ replicate (n - 1) ','  ++ ")"

tupleType :: [Type] -> Type
tupleType tss = foldl TyApp (TyCon (tupleCon (length tss))) tss

tupleTerm :: [Term fn lt] -> Term fn lt
tupleTerm ts = foldl App (Con (tupleCon (length ts))) ts
