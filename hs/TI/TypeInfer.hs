{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             FunctionalDependencies #-}
module TI.TypeInfer where

import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State
import Data.List ((\\))

import Syntax
import Fresh
import TI.TypesTypes
import TI.BaseEnv

-------------------------------------------------------------------------------

class MonadInfer m => MonadUnify ty m | m -> ty where
    applySubst :: SubstApply ty a => a -> m a
    extSubst   :: SubstApply ty ty => Subst ty -> m ()
    getSubst   :: m (Subst ty)

    freshVar   :: m Var

unify :: MonadUnify Type m => Type -> Type -> m ()
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

instance MonadInfer (StateT s (StateT InferState (ErrorT TypeError (Fresh Integer)))) where
    getTypes = lift getTypes
    putTypes = lift . putTypes

    getKinds = lift getKinds
    putKinds = lift . putKinds

instance MonadUnify ty
         (StateT (Subst ty) (StateT InferState (ErrorT TypeError (Fresh Integer)))) where
    applySubst t = liftM (`apply` t) get
    extSubst sub' = modify (@@ sub')
    getSubst = get

    freshVar = liftM (\i -> var ("_v" ++ show i)) fresh

-------------------------------------------------------------------------------

updateTypes :: MonadUnify Type m => m ()
updateTypes = do
    sub <- getSubst
    types <- getTypes
    putTypes (apply sub types)

freshTyVar :: MonadUnify Type m => Kind -> m Type
freshTyVar k = do
    ty <- freshVar
    addKind (unVar ty) k
    return (TyVar ty)

freshen :: MonadUnify Type m => Scheme -> m Type
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
    ty <- freshTyVar Star
    ty1 <- tiTerm t1
    updateTypes
    ty2 <- tiTerm t2
    (`unify` (ty2 --> ty)) =<< applySubst ty1
    applySubst ty
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

-- inferType :: [Decl DTerm] -> Either TypeError ([Assump Scheme], [Assump Kind])
-- inferType decls' =
--     fmap (\(tys, ks) -> (tys \\ baseTypes, ks \\ baseKinds)) $
--     evalFresh (runErrorT (go baseTypes baseKinds decls')) (0 :: Integer)
--   where
--     go tys ks [] = return (reverse tys, reverse ks)
--     go tys ks (decl : decls) = do
--         InferState _ tys' ks' <- execStateT (tyDecl decl) (InferState [] tys ks)
--         go tys' ks' decls

-- tyDecl :: MonadInfer m => Decl DTerm -> m ()
-- tyDecl (ValDecl v t) = do
--     tys <- getTypes
--     tyv <- freshTyVar Star
--     addType (v :>: toScheme tyv)
--     ty <- tyTerm t
--     (`unify` ty) =<< applySubst tyv
--     ty' <- applySubst ty
--     putTypes ((v :>: toScheme ty') : tys)
-- tyDecl (DataDecl tyc tyvs body) = tyDataDecl tyc tyvs body

-- getTSKinds :: MonadError TypeError m => TypeSig -> m [(Id, Kind)]
-- getTSKinds ts =
--     forM varsKinds $ \varKinds ->
--         case varKinds of
--             ((tyv, k1) : _) -> case filter (/= k1) (map snd varKinds) of
--                 []       -> return (tyv, k1)
--                 (k2 : _) -> throwError $ MismatchingKinds tyv k1 k2
--             _ -> error "TI.TypeInfer.getTSKinds: empty list, something went wrong"
--   where
--     varsKinds :: [[(Id, Kind)]]
--     varsKinds =  groupBy ((==) `on` fst) $ sortBy (comparing fst) (go Star ts)

--     go :: Kind -> TypeSig -> [(Id, Kind)]
--     go k (TyVar tyv) = [(tyv, k)]
--     go k (TyApp ts1 ts2) = go (Star :*> k) ts1 ++ go Star ts2
--     go _ _ = []

-- tyDataDecl :: MonadInfer m => Id -> [Id] -> DataBody -> m ()
-- tyDataDecl tyc tyvs body = do
--     let ts = foldl TyApp (TyCon tyc) $ map TyVar tyvs
--     (ks, tss) <- tyDataBody ts tyvs [] [] body
--     let ks' = flip map tyvs $ \v -> case lookup v ks of
--                   Nothing -> Star
--                   Just k' -> k'
--         k = foldl (:*>) Star ks'
--     ksCtx <- getKinds
--     putKinds ((tyc :>: k) : ksCtx)

--     forM_ tss $ \(tyc2, ts2) -> do
--         ty <- tyToTs ks ts2
--         ctx <- getTypes
--         putTypes ((tyc2 :>: quantify (fv ty) ty) : ctx)

-- tyToTs :: MonadInfer m
--           => [(Id, Kind)]        -- * The type variables kinds
--           -> TypeSig -> m Type
-- tyToTs _ (TyCon tyc) = do
--     ksCtx <- getKinds
--     case lookupAss tyc ksCtx of
--         Nothing -> throwError $ UnboundTypeConstructor tyc
--         Just k  -> return $ TyCon (tyc, k)
-- tyToTs ks (TyVar tyv) = case lookup tyv ks of
--     Nothing -> error "TypeInfer.tyDataBody: something went wrong, var lookup"
--     Just k  -> return $ TyVar (tyv, k)
-- tyToTs ks (TyApp ty1 ty2) = liftM2 TyApp (tyToTs ks ty1) (tyToTs ks ty2)
-- tyToTs _ (TyGen i) = return $ TyGen i

-- tyDataBody :: MonadInfer m
--               => TypeSig         -- * The type signature of the type constructor
--               -> [Id]            -- * The type variables of the type constructor
--               -> [(Id, Kind)]    -- * The kinds of the type variables up to now
--                                 --   (first result)
--               -> [(Id, TypeSig)] -- * The type signatures of the data constructors
--                                 --   up to now (second result)
--               -> DataBody
--               -> m ([(Id, Kind)], [(Id, TypeSig)])
-- tyDataBody _ _ ks tss [] = return (ks, tss)
-- tyDataBody tsOrig tyvs ks tss' ((c, tss) : body) = do
--     let ts = foldr (\ty -> TyApp (TyApp (TyCon "(->)") ty)) tsOrig tss
--     ks' <- getTSKinds ts
--     forM_ ks' $ \(tyv, k) -> case lookup tyv ks of
--         Nothing -> if tyv `elem` tyvs then return ()
--                    else throwError $ UnboundTypeVar tyv
--         Just k' -> if k == k' then return ()
--                    else throwError $ MismatchingKinds tyv k k'
--     let ks'' = ks `union` ks'
--     tyDataBody tsOrig tyvs ks'' ((c, ts) : tss') body
