{-# LANGUAGE FlexibleInstances, FlexibleContexts, TupleSections, ScopedTypeVariables #-}
module TI.TypeInfer where

import Control.Monad.Error
import Control.Monad.State
import Data.List ((\\), groupBy, sortBy, union)
import Data.Ord (comparing)
import Data.Function (on)

import Syntax
import Fresh
import TI.TypesTypes
import TI.BaseEnv

inferType :: [Decl DTerm] -> Either TypeError ([Assump Scheme], [Assump Kind])
inferType decls' =
    fmap (\(tys, ks) -> (tys \\ baseTypes, ks \\ baseKinds)) $
    evalFresh (runErrorT (go baseTypes baseKinds decls')) (0 :: Integer)
  where
    go tys ks [] = return (reverse tys, reverse ks)
    go tys ks (ValDecl v t : decls) = do
        ty <- evalStateT (tyTerm t) (InferState [] tys ks)
        go ((v :>: quantify (fv ty) ty) : tys) ks decls
    go tys ks (DataDecl tyc tyvs body : decls) = do
        InferState _ tys' ks' <-
            execStateT (tyDataDecl tyc tyvs body) (InferState [] tys ks)
        go tys' ks' decls

class (MonadFresh Integer m, MonadError TypeError m) => MonadInfer m where
    applySubst :: Types ty => ty -> m ty
    extSubst   :: Subst -> m ()

    getTypes   :: m [Assump Scheme]
    putTypes   :: [Assump Scheme] -> m ()

    getKinds   :: m [Assump Kind]
    putKinds   :: [Assump Kind] -> m ()

    unify      :: Type -> Type -> m ()

data InferState = InferState { substitution :: Subst
                             , types        :: [Assump Scheme]
                             , kinds        :: [Assump Kind]
                             }
                deriving (Eq, Show)

instance MonadInfer (StateT InferState (ErrorT TypeError (Fresh Integer))) where
    applySubst ty = liftM (`apply` ty) (gets substitution)
    extSubst sub =
        modify (\(InferState sub' tys ks) -> (InferState (sub @@ sub') tys ks))

    getTypes = gets types
    putTypes tys = modify (\(InferState s _ ks) -> (InferState s tys ks))

    getKinds = gets kinds
    putKinds ks = modify (\(InferState s tys _) -> (InferState s tys ks))

    unify ty1 ty2 =
        do sub1 <- gets substitution
           sub2 <- mgu (apply sub1 ty1) (apply sub1 ty2)
           extSubst sub2

runInferMonad :: StateT InferState (ErrorT TypeError (Fresh Integer)) a -> InferState -> Either TypeError (a, InferState)
runInferMonad m s = evalFresh (runErrorT (runStateT m s)) (0 :: Integer)

lookupTypes :: MonadInfer m => Id -> m (Maybe Scheme)
lookupTypes tyv = liftM (lookupAss tyv) getTypes

addType :: MonadInfer m => Assump Scheme -> m ()
addType scs = do {ctx <- getTypes; putTypes (scs : ctx)}

updateTypes :: MonadInfer m => m ()
updateTypes = do {ctx <- getTypes; putTypes =<< applySubst ctx}

freshTyVar :: (Show c, MonadFresh c m) => Kind -> m Type
freshTyVar k = liftM (\i -> TyVar ("_ty" ++ show i, k)) fresh

freshen :: (Show c, MonadFresh c m) => Scheme -> m Type
freshen (Forall ks t) = liftM (`inst` t) (mapM freshTyVar ks)

tyTerm :: MonadInfer m => DTerm -> m Type
tyTerm (Var v) = do
    tyM <- lookupTypes v
    case tyM of
        Just ty -> freshen ty
        Nothing -> throwError (UnboundVar v)
tyTerm (Con c) = do
    tyM <- lookupTypes c
    case tyM of
        Just ty -> freshen ty
        Nothing -> throwError (UnboundConstructor c)
tyTerm (Abs v t) = do
    tyv <- freshTyVar Star
    addType (v :>: toScheme tyv)
    ty1 <- tyTerm t
    applySubst (tyv --> ty1)
tyTerm (App t1 t2) = do
    tyv <- freshTyVar Star
    ty1 <- tyTerm t1
    updateTypes
    ty2 <- tyTerm t2
    (`unify` (ty2 --> tyv)) =<< applySubst ty1
    applySubst tyv
tyTerm (Let v t1 t2) = do
    ty <- tyTerm t1
    updateTypes
    ctxFv <- liftM fv getTypes
    let ty' = quantify (fv ty \\ ctxFv) ty
    addType (v :>: ty')
    tyTerm t2
tyTerm (Fix v t) = do
    tyv <- freshTyVar Star
    addType (v :>: toScheme tyv)
    ty <- tyTerm t
    (`unify` ty) =<< applySubst tyv
    applySubst ty
tyTerm (Literal l) = return (tyLit l)
tyTerm (Case t cases) = do
    ty <- tyTerm t
    tyv <- freshTyVar Star
    tyCases ty tyv cases

tyCases :: MonadInfer m
           => Type               -- * The type of the matched term
           -> Type               -- * The return type
           -> [(Pattern, DTerm)] -> m Type
tyCases _ ty [] = return ty
tyCases tyT ty1 ((pt, t) : cases) = do
    tyPt <- tyPattern pt
    unify tyPt tyT
    ty1' <- applySubst ty1
    tyT' <- applySubst tyT
    ty2 <- tyTerm t
    unify ty1' ty2
    ty1'' <- applySubst ty1'
    tyCases tyT' ty1'' cases

tyPattern :: MonadInfer m => Pattern -> m Type
tyPattern (VarPat v) = do
    tyv <- freshTyVar Star
    addType (v :>: toScheme tyv)
    return tyv
tyPattern (LitPat lit) = return (tyLit lit)
tyPattern (Pat c pts) = do
    tyM <- lookupTypes c
    case tyM of
        Nothing -> throwError (UnboundConstructor c)
        Just sc -> do
            tys <- mapM tyPattern pts
            tyv <- freshTyVar Star
            ty <- freshen sc
            unify ty (foldr (-->) tyv tys)
            applySubst tyv

tyLit :: Literal -> Type
tyLit (IntLit _) = intCon
tyLit (RealLit _) = realCon

getTSKinds :: MonadError TypeError m => TypeSig -> m [(Id, Kind)]
getTSKinds ts =
    forM varsKinds $ \varKinds ->
        case varKinds of
            ((tyv, k1) : _) -> case filter (/= k1) (map snd varKinds) of
                []       -> return (tyv, k1)
                (k2 : _) -> throwError $ MismatchingKinds tyv k1 k2
            _ -> error "TI.TypeInfer.getTSKinds: empty list, something went wrong"
  where
    varsKinds :: [[(Id, Kind)]]
    varsKinds =  groupBy ((==) `on` fst) $ sortBy (comparing fst) (go Star ts)

    go :: Kind -> TypeSig -> [(Id, Kind)]
    go k (TyVar tyv) = [(tyv, k)]
    go k (TyApp ts1 ts2) = go (Star :*> k) ts1 ++ go Star ts2
    go _ _ = []

tyDataDecl :: MonadInfer m => Id -> [Id] -> DataBody -> m ()
tyDataDecl tyc tyvs body = do
    let ts = foldl TyApp (TyCon tyc) $ map TyVar tyvs
    (ks, tss) <- tyDataBody ts tyvs [] [] body
    let ks' = flip map tyvs $ \v -> case lookup v ks of
                  Nothing -> Star
                  Just k' -> k'
        k = foldl (:*>) Star ks'
    ksCtx <- getKinds
    putKinds ((tyc :>: k) : ksCtx)

    forM_ tss $ \(tyc2, ts2) -> do
        ty <- tyToTs ks ts2
        ctx <- getTypes
        putTypes ((tyc2 :>: quantify (fv ty) ty) : ctx)

tyToTs :: MonadInfer m
          => [(Id, Kind)]        -- * The type variables kinds
          -> TypeSig -> m Type
tyToTs _ (TyCon tyc) = do
    ksCtx <- getKinds
    case lookupAss tyc ksCtx of
        Nothing -> throwError $ UnboundTypeConstructor tyc
        Just k  -> return $ TyCon (tyc, k)
tyToTs ks (TyVar tyv) = case lookup tyv ks of
    Nothing -> error "TypeInfer.tyDataBody: something went wrong, var lookup"
    Just k  -> return $ TyVar (tyv, k)
tyToTs ks (TyApp ty1 ty2) = liftM2 TyApp (tyToTs ks ty1) (tyToTs ks ty2)
tyToTs _ (TyGen i) = return $ TyGen i

tyDataBody :: MonadInfer m
              => TypeSig         -- * The type signature of the type constructor
              -> [Id]            -- * The type variables of the type constructor
              -> [(Id, Kind)]    -- * The kinds of the type variables up to now
                                --   (first result)
              -> [(Id, TypeSig)] -- * The type signatures of the data constructors
                                --   up to now (second result)
              -> DataBody
              -> m ([(Id, Kind)], [(Id, TypeSig)])
tyDataBody _ _ ks tss [] = return (ks, tss)
tyDataBody tsOrig tyvs ks tss' ((c, tss) : body) = do
    let ts = foldr (\ty -> TyApp (TyApp (TyCon "(->)") ty)) tsOrig tss
    ks' <- getTSKinds ts
    forM_ ks' $ \(tyv, k) -> case lookup tyv ks of
        Nothing -> if tyv `elem` tyvs then return ()
                   else throwError $ UnboundTypeVar tyv
        Just k' -> if k == k' then return ()
                   else throwError $ MismatchingKinds tyv k k'
    let ks'' = ks `union` ks'
    tyDataBody tsOrig tyvs ks'' ((c, ts) : tss') body
