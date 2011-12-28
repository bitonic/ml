{-# LANGUAGE TupleSections #-}
module Desugar (desugar) where

import Control.Monad (liftM, liftM2)

import Fresh
import Syntax

desugar :: [Decl FullTerm] -> [Decl DTerm]
desugar decls = evalFresh (mapM dDecl decls) (0 :: Integer)

dDecl :: (MonadFresh c m, Show c) => Decl FullTerm -> m (Decl DTerm)
dDecl (ValDecl v t) = liftM (ValDecl v) (dTerm t)
dDecl (TypeSig v ts) = return $ TypeSig v ts
dDecl (DataDecl c tyvs body) = return $ DataDecl c tyvs body

freshVar :: (Show c, MonadFresh c m) => m Var
freshVar = liftM (VarN . ("_v" ++) . show) fresh

dTerm :: (Show c, MonadFresh c m) => FullTerm -> m DTerm
dTerm (Abs pts t) = go pts
  where
    go [] = dTerm t
    go (pt : pts') = case pt of
        VarPat v -> liftM (Abs v) (go pts')
        _ -> do v <- freshVar
                liftM (Abs v) $ dTerm (Case (Var v) [(pt, Abs pts' t)])
dTerm (Let pt t1 t2) = do
    t1' <- dTerm t1
    t2' <- dTerm t2
    case pt of
        VarPat v -> return (Let v t1' t2')
        _ -> do v <- freshVar
                liftM (Let v t1') (dTerm (Case (Var v) [(VarPat v, t2)]))
dTerm (Case t' cases) = liftM2 Case (dTerm t') (mapM dCase cases)
  where
    dCase (pt, t) = liftM (pt,) (dTerm t)
dTerm (Var v) = return (Var v)
dTerm (Con c) = return (Con c)
dTerm (Literal lit) = return (Literal lit)
dTerm (App t1 t2) = liftM2 App (dTerm t1) (dTerm t2)
