{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             FunctionalDependencies, IncoherentInstances, ScopedTypeVariables #-}
module TI.TypeInfer where

import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State
import Data.List ((\\))
import qualified Data.Map as Map

import Syntax
import Fresh
import TI.TypesTypes
import TI.BaseEnv

import Debug.Trace
import Pretty

-------------------------------------------------------------------------------

class MonadInfer m => MonadUnify ty m | m -> ty where
    applySubst :: SubstApply ty a => a -> m a
    extSubst   :: SubstApply ty ty => Subst ty -> m ()
    getSubst   :: m (Subst ty)

unify :: (Unifiable ty, MonadUnify ty m) => ty -> ty -> m ()
unify ty1 ty2 = do
    sub1 <- getSubst
    sub2 <- mgu (apply sub1 ty1) (apply sub1 ty2)
    extSubst sub2

--- INSTANCES -----------------------------------------------------------------

type InferState = (Assump Scheme, Assump Kind)

instance MonadInfer (StateT InferState (ErrorT TypeError (Fresh Integer))) where
    getTypes = gets fst
    putTypes tys = modify $ \(_, ks) -> (tys, ks)

    getKinds = gets snd
    putKinds ks = modify $ \(tys, _) -> (tys, ks)

    freshVar = liftM (\i -> var ("_v" ++ show i)) fresh

instance MonadInfer m => MonadInfer (StateT a m) where
    getTypes = lift getTypes
    putTypes = lift . putTypes

    getKinds = lift getKinds
    putKinds = lift . putKinds

    freshVar = lift freshVar

instance MonadInfer m => MonadUnify ty (StateT (Subst ty) m) where
    applySubst t  = liftM (`apply` t) get
    extSubst sub' = modify (@@ sub')
    getSubst      = get

-------------------------------------------------------------------------------

updateTypes :: MonadUnify Type m => m ()
updateTypes = do
    sub <- getSubst
    types <- getTypes
    putTypes (apply sub types)

freshTyVar :: MonadInfer m => Kind -> m Type
freshTyVar k = do
    ty <- freshVar
    addKind (unVar ty) k
    return (TyVar ty)

freshen :: MonadInfer m => Scheme -> m Type
freshen (Forall ks t) = liftM (`inst` t) (mapM freshTyVar ks)

tiTerm :: (MonadInfer m, MonadUnify Type m) => DTerm -> m Type
tiTerm (Var v) = lookupVar v >>= freshen
tiTerm (Con c) = lookupCon c >>= freshen
tiTerm (Abs v t) = do
    ty <- freshTyVar Star
    addVar v (toScheme ty)
    ty1 <- tiTerm t
    applySubst (ty --> ty1)
tiTerm (App t1 t2) = do
    tyv <- freshTyVar Star
    ty1 <- tiTerm t1
    updateTypes
    ty2 <- tiTerm t2
    (`unify` (ty2 --> tyv)) =<< applySubst ty1
    applySubst tyv
tiTerm (Let v t1 t2) = do
    ty <- tiTerm t1
    updateTypes
    ctxFv <- liftM fv getTypes
    ty' <- quantify (fv ty \\ ctxFv) ty
    addVar v ty'
    tiTerm t2
tiTerm (Literal lit) = return $ tiLiteral lit
tiTerm (Case t cases) = do
    ty <- tiTerm t
    tyv <- freshTyVar Star
    tiCases ty tyv cases

tiCases :: MonadUnify Type m
           => Type               -- * The type of the matched term
           -> Type               -- * The return type
           -> [(Pattern, DTerm)] -> m Type
tiCases _ ty [] = return ty
tiCases tyT ty1 ((pt, t) : cases) = do
    tyPt <- tiPattern pt
    unify tyPt tyT
    ty2 <- tiTerm t
    unify ty1 ty2
    ty1' <- applySubst ty1
    tyT' <- applySubst tyT
    tiCases tyT' ty1' cases

tiLiteral :: Literal -> Type
tiLiteral (IntLit _) = intCon
tiLiteral (RealLit _) = realCon

tiPattern :: MonadUnify Type m => Pattern -> m Type
tiPattern (VarPat v) = do
    ty <- freshTyVar Star
    addVar v (toScheme ty)
    return ty
tiPattern (IntPat _) = return intCon
tiPattern (Pat c pts) = do
    sc <- lookupCon c
    tys <- mapM tiPattern pts
    tyv <- freshTyVar Star
    ty <- freshen sc
    unify ty (foldr (-->) tyv tys)
    applySubst tyv

updateKinds :: MonadUnify Kind m => m ()
updateKinds = do
    sub <- getSubst
    kinds <- getKinds
    putKinds (apply sub kinds)

kiType :: MonadUnify Kind m => Type -> m Kind
kiType (TyVar v) = lookupTyVar v
kiType (TyCon c) = lookupTyCon c
kiType (TyApp t1 t2) = do
    kv <- liftM KVar freshVar
    k1 <- kiType t1
    updateKinds
    k2 <- kiType t2
    (`unify` (k2 :*> kv)) =<< applySubst k1
    updateKinds
    applySubst kv
kiType (TyGen _) = error "TI.TypeInfer.kiType: TyGen"

tiDataOption :: MonadUnify Kind m => Type -> [Type] -> m Type
tiDataOption res tys = do
    forM_ tys $ \ty -> do
        k <- kiType ty
        sub <- getSubst
        unify k Star
        applySubst k
    return (foldr (-->) res tys)

replaceVars :: Kind -> Kind
replaceVars (KVar _) = Star
replaceVars (k1 :*> k2) = replaceVars k1 :*> replaceVars k2
replaceVars k = k

kiDataDecl :: MonadUnify Kind m => Con -> [Var] -> DataBody -> m Kind
kiDataDecl tyc tyvs body = do
    forM_ tyvs $ \tyv -> do
        kv <- liftM KVar freshVar
        addTyVar tyv kv

    let res = foldl TyApp (TyCon tyc) $ map TyVar tyvs

    addTyCon tyc =<< liftM KVar freshVar

    tys <- forM body $ \(c, tys) -> do
        ty <- tiDataOption res tys
        return (c, ty)

    kCtx <- getKinds
    putKinds (Map.map replaceVars kCtx)

    forM_ tys $ \(c, ty) -> addCon c =<< quantify (fv ty) ty

    ks <- mapM lookupTyVar tyvs

    return $ foldr (:*>) Star ks

tiDecl :: MonadInfer m => Decl DTerm -> m ()
tiDecl (ValDecl v t) = do
    tys <- getTypes
    ks <- getKinds
    ty <- evalStateT term ([] :: Subst Type)
    sc <- quantify (fv ty) ty
    putKinds ks
    putTypes tys
    addVar v sc
  where
    term = do
        tyv <- freshTyVar Star
        addVar v (toScheme tyv)
        ty <- tiTerm t
        (`unify` ty) =<< applySubst tyv
        applySubst ty
tiDecl (DataDecl tyc tyvs body) = do
    ks <- getKinds
    k <- evalStateT (kiDataDecl tyc tyvs body) ([] :: Subst Kind)
    putKinds ks
    addTyCon tyc k

-------------------------------------------------------------------------------

typeInfer :: [Decl DTerm] -> Either TypeError (Assump Scheme, Assump Kind)
typeInfer decls =
    fmap (\(tys, ks) -> (Map.difference tys baseTypes, Map.difference ks baseKinds)) res
  where
    res :: Either TypeError InferState =
        evalFresh (runErrorT (execStateT (mapM_ tiDecl decls) (baseTypes, baseKinds))) (0 :: Integer)
