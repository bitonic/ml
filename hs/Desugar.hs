{-# LANGUAGE TupleSections #-}
module Desugar 
       ( Decl (..)
       , Term (..)
       , Pattern (..)
       , Literal (..)
       , DataBody
       , desugar
       ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM, liftM2)
import Data.Traversable (traverse)

import Fresh
import Lexer (Id)
import Parser (Literal (..), DataBody)
import qualified Parser as P

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
             | LitPat (Literal Id)
             deriving (Show, Eq)

desugar :: [P.Decl] -> [Decl]
desugar decls = evalFresh (mapM dDecl decls) (0 :: Integer)

dDecl :: (MonadFresh c m, Applicative m, Show c) => P.Decl -> m Decl
dDecl (P.ValDecl v t) = liftM (ValDecl v) (dTerm t)
dDecl (P.DataDecl con tvs body) = return (DataDecl con tvs body)

freshVar :: (Show c, MonadFresh c m) => m Id
freshVar = liftM (('v' :) . show) fresh

depat :: (Show c, MonadFresh c m, Applicative m)
         => P.Pattern -> P.Term -> (Id -> Term -> Term)
         -> ((Term -> Term) -> m Term) -> m Term
depat pt t f alt = case pt of
    P.VarPat v -> alt (f v)
    _ -> do
        v <- freshVar
        liftM (f v) (dTerm (P.Case (P.Var v) [(pt, t)]))

depatS :: (Show c, MonadFresh c m, Applicative m)
          => P.Pattern -> P.Term -> m (Id, Term)
depatS pt t = case pt of
    P.VarPat v -> liftM (v,) (dTerm t)
    _ -> do
        v <- freshVar
        liftM (v,) (dTerm (P.Case (P.Var v) [(pt, t)]))

dTerm :: (Show c, MonadFresh c m, Applicative m) => P.Term -> m Term
dTerm (P.Abs pts t) = go pts
  where
    go [] = dTerm t
    go (pt : pts') = depat pt (P.Abs pts' t) Abs (`liftM` (go pts'))
dTerm (P.Let pt t1 t2) = do
    dt1 <- dTerm t1
    dt2 <- dTerm t2
    depat pt t2 (\v -> Let v dt1) (`liftM` (return dt2))
dTerm (P.Case term cases) = liftM2 Case (dTerm term) (mapM dCase cases)
  where
    dCase (P.VarPat v, t) = liftM (VarPat v,) (dTerm t)
    dCase (P.Pat con Nothing, t) = liftM (Pat con Nothing,) (dTerm t)
    dCase (P.Pat con (Just pt), t) = do
        (v, t') <- depatS pt t
        return (Pat con (Just v), t')
    dCase (P.LitPat (TupleLit pts), t) = do
        (vs, t') <- dTupLit pts t
        liftM (LitPat (TupleLit vs),) (dTerm t')
    dCase (P.LitPat (IntLit i), t) = liftM (LitPat (IntLit i),) (dTerm t)
    dCase (P.LitPat (RealLit r), t) = liftM (LitPat (RealLit r),) (dTerm t)

    dTupLit [] t = return ([] :: [Id], t)
    dTupLit (pt : pts) t = do
        (vs, t') <- dTupLit pts t
        case pt of
            P.VarPat v -> return (v : vs, t')
            _ -> do
                v <- freshVar
                return (v : vs, P.Case (P.Var v) [(pt, t')])

dTerm (P.Var v) = return (Var v)
dTerm (P.Fix f t) = liftM (Fix f) (dTerm t)
dTerm (P.Literal lit) = liftM Literal (traverse dTerm lit)
dTerm (P.App l r) = liftM2 App (dTerm l) (dTerm r)
