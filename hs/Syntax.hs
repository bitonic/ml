{-# LANGUAGE OverloadedStrings #-}
module Syntax
       ( Id
       , Decl (..)
       , DataBody
       , TypeS (..)
       , TypeSig
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

import Text.PrettyPrint

-------------------------------------------------------------------------------

type Id = String

data Decl t = ValDecl Id t
            | DataDecl Id [Id] DataBody
            deriving (Show, Eq)

data TypeS t = TyCon t
             | TyApp (TypeS t) (TypeS t)
             | TyVar t
             | TyGen Int
            deriving (Show, Eq)

type TypeSig = TypeS Id

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

type DataBody = [(Id, [TypeSig])]

-------------------------------------------------------------------------------

type FullTerm = Term [Pattern] Pattern

type DTerm    = Term Id Id

-------------------------------------------------------------------------------

tupleCon :: Int -> String
tupleCon n = "(" ++ replicate (n - 1) ','  ++ ")"

tupleType :: [TypeSig] -> TypeSig
tupleType tss = foldl TyApp (TyCon (tupleCon (length tss))) tss

tupleTerm :: [Term fn lt] -> Term fn lt
tupleTerm ts = foldl App (Con (tupleCon (length ts))) ts
