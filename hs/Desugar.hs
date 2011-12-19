{-# LANGUAGE TupleSections #-}
module Desugar where

import Control.Applicative (Applicative)
import Control.Monad (liftM, liftM2)
import Data.Traversable (traverse)

import Fresh
import Lexer (Id)
import qualified Parser as P
import Parser (Literal (..), DataBody)

data Decl = ValDecl Id Term
          | DataDecl Id [Id] DataBody
          deriving (Show, Eq)

data Term = Var Id
          | Abs Id Term
          | App Term Term
          | Let Id Term Term
          | Fix Id Term
          | Literal (Literal Term)
          | Case Term [(Pattern, Term)]
          deriving (Show, Eq)

data Pattern = VarPat Id
             | Pat Id (Maybe Id)
             | LitPat (Literal Pattern)
             deriving (Show, Eq)

desugar :: [P.Decl] -> [Decl]
desugar = undefined

dDecl :: (MonadFresh c m, Applicative m, Show c) => P.Decl -> m Decl
dDecl (P.ValDecl v t) = liftM (ValDecl v) (dTerm t)
dDecl (P.DataDecl con tvs body) = return (DataDecl con tvs body)

freshVar :: (Show c, MonadFresh c m) => m Id
freshVar = liftM (('v' :) . show) fresh

dTerm :: (Show c, MonadFresh c m, Applicative m) => P.Term -> m Term
dTerm (P.Abs pts t) = go pts
  where
    go [] = dTerm t
    go (pt : pts') = case pt of
        P.VarPat v -> liftM (Abs v) (go pts')
        pt' -> do
            v <- freshVar
            let rest = P.Abs pts' t
            liftM (Abs v) (dTerm (P.Case (P.Var v) [(pt', rest)]))
dTerm (P.Let pt t1 t2) = do
    dt1 <- dTerm t1
    dt2 <- dTerm t2
    case pt of
        P.VarPat v -> return (Let v dt1 dt2)
        pt -> do
            v <- freshVar
            liftM (Let v dt1) (dTerm (P.Case (P.Var v) [(pt, t2)]))
dTerm (P.Case t' cases) = liftM2 Case (dTerm t') (mapM dCase cases)
  where
    dCase (P.VarPat v, t) = liftM (VarPat v,) (dTerm t)
    dCase (P.Pat con Nothing, t) = liftM (Pat con Nothing,) (dTerm t)
    dCase (P.Pat con (Just pt), t) = undefined
    dCase (P.LitPat (TupleLit pts), t) = undefined
    dCase (P.LitPat (IntLit i), t) = liftM (LitPat (IntLit i),) (dTerm t)
    dCase (P.LitPat (RealLit r), t) = liftM (LitPat (RealLit r),) (dTerm t)
dTerm (P.Var v) = return (Var v)
dTerm (P.Fix f t) = liftM (Fix f) (dTerm t)
dTerm (P.Literal lit) = liftM Literal (traverse dTerm lit)
dTerm (P.App l r) = liftM2 App (dTerm l) (dTerm r)
