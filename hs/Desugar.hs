{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Desugar
       ( Decl (..)
       , Term (..)
       , DTerm
       , Pattern (..)
       , Literal (..)
       , DataBody
       , TypeSig
       , desugar
       , prettyDesugar
       ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM, liftM2)
import Text.PrettyPrint

import Fresh
import Lexer (Id)
import Parser

type FullTerm = Term [Pattern] Pattern
type DTerm    = Term Id Id

desugar :: [Decl FullTerm] -> [Decl DTerm]
desugar decls = evalFresh (mapM dDecl decls) (0 :: Integer)

dDecl :: (MonadFresh c m, Applicative m, Show c)
         => Decl FullTerm -> m (Decl DTerm)
dDecl (ValDecl v t) = liftM (ValDecl v) (dTerm t)
dDecl (DataDecl con tvs body) = return (DataDecl con tvs body)

freshVar :: (Show c, MonadFresh c m) => m Id
freshVar = liftM (("_v" ++) . show) fresh

dTerm :: (Show c, MonadFresh c m, Applicative m) =>
         FullTerm -> m DTerm
dTerm (Abs pts t) = go pts
  where
    go [] = dTerm t
    go (pt : pts') = case pt of
        VarPat v -> liftM (Abs v) (go pts')
        _ -> do v <- freshVar
                liftM (Abs v) $ dTerm (Case (Var v) [(pt, Abs pts' t)])
dTerm (Let pt t1 t2) = do
    dt1 <- dTerm t1
    dt2 <- dTerm t2
    case pt of
        VarPat v -> return (Let v dt1 dt2)
        _ -> do v <- freshVar
                liftM (Let v dt1) (dTerm (Case (Var v) [(VarPat v, t2)]))
dTerm (Case term cases) = liftM2 Case (dTerm term) (mapM dCase cases)
  where
    dCase (pt, t) = liftM (pt,) (dTerm t)
dTerm (Var v) = return (Var v)
dTerm (Con c) = return (Con c)
dTerm (Fix f t) = liftM (Fix f) (dTerm t)
dTerm (Literal lit) = return (Literal lit)
dTerm (App l r) = liftM2 App (dTerm l) (dTerm r)

prettyDesugar :: [Decl DTerm] -> String
prettyDesugar = render . vcat . map (pDecl text text)
