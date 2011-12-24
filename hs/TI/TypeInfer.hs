{-# LANGUAGE FlexibleInstances, FlexibleContexts, TupleSections, ScopedTypeVariables #-}
module TI.TypeInfer (inferType) where

import Control.Monad.Error
import Control.Monad.State
import Data.List ((\\), groupBy, sortBy, union)
import Data.Ord (comparing)
import Data.Function (on)

import Syntax
import Fresh
import TI.TypesTypes
import TI.BaseEnv

import Debug.Trace

inferType :: [Decl DTerm] -> Either TypeError ([Assump Scheme], [Assump Kind])
inferType decls' = fmap (\(ts, ks) -> (ts \\ baseTypes, ks \\ baseKinds)) $
                   evalFresh (runErrorT (go baseTypes baseKinds decls')) (0 :: Integer)
  where
    go ts ks [] = return (reverse ts, reverse ks)
    go ts ks (ValDecl v t : decls) = do
        ty <- evalStateT (tyTerm t) (InferState [] ts ks)
        go ((v :>: quantify (fv ty) ty) : ts) ks decls
    go ts ks (DataDecl con vars body : decls) = do
        InferState _ ts' ks' <- execStateT (tyDataDecl con vars body) (InferState [] ts ks)
        go ts' ks' decls

class (MonadFresh Integer m, MonadError TypeError m) => MonadInfer m where
    applySubst :: Types t => t -> m t
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
    applySubst t = liftM (`apply` t) (gets substitution)
    extSubst s = modify (\(InferState s' ts ks) -> (InferState (s @@ s') ts ks))

    getTypes = gets types
    putTypes ts = modify (\(InferState s _ ks) -> (InferState s ts ks))

    getKinds = gets kinds
    putKinds ks = modify (\(InferState s ts _) -> (InferState s ts ks))

    unify t1 t2 = do s1 <- gets substitution
                     s2 <- mgu (apply s1 t1) (apply s1 t2)
                     extSubst s2

lookupTypes :: MonadInfer m => Id -> m (Maybe Scheme)
lookupTypes v = liftM (lookupAss v) getTypes

addType :: MonadInfer m => Assump Scheme -> m ()
addType ass = do {ctx <- getTypes; putTypes (ass : ctx)}

updateTypes :: MonadInfer m => m ()
updateTypes = do {ctx <- getTypes; putTypes =<< applySubst ctx}

freshTyVar :: (Show c, MonadFresh c m) => Kind -> m Type
freshTyVar k = liftM (\i -> TyVar ("_v" ++ show i, k)) fresh

freshen :: (Show c, MonadFresh c m) => Scheme -> m Type
freshen (Forall ks t) = liftM (`inst` t) (mapM freshTyVar ks)

tyTerm :: MonadInfer m => DTerm -> m Type
tyTerm (Var v) = do
    tM <- lookupTypes v
    case tM of
        Just t -> freshen t
        Nothing -> throwError (UnboundVar v)
tyTerm (Con c) = do
    tM <- lookupTypes c
    case tM of
        Just t -> freshen t
        Nothing -> throwError (UnboundConstructor c)
tyTerm (Abs v t) = do
    tv <- freshTyVar Star
    addType (v :>: toScheme tv)
    a <- tyTerm t
    applySubst (tv --> a)
tyTerm (App l r) = do
    tv <- freshTyVar Star
    a <- tyTerm l
    updateTypes
    b <- tyTerm r
    (`unify` (b --> tv)) =<< applySubst a
    applySubst tv
tyTerm (Let v t1 t2) = do
    a <- tyTerm t1
    updateTypes
    ctxFv <- liftM fv getTypes
    let a' = quantify (fv a \\ ctxFv) a
    addType (v :>: a')
    tyTerm t2
tyTerm (Fix v t) = do
    tv <- freshTyVar Star
    addType (v :>: toScheme tv)
    a <- tyTerm t
    (`unify` a) =<< applySubst tv
    applySubst a
tyTerm (Literal l) = return (tyLit l)
tyTerm (Case t cases) = tyTerm t >>= (`tyCases` cases)

tyCases :: MonadInfer m => Type -> [(Pattern, DTerm)] -> m Type
tyCases ty [] = return ty
tyCases ty (cs : css) = do
    ty' <- tyCase ty cs
    unify ty ty'
    ty'' <- applySubst ty'
    tyCases ty'' css

tyCase :: MonadInfer m => Type -> (Pattern, DTerm) -> m Type
tyCase ty (pt, t) = do
    typt <- tyPattern pt
    unify ty typt
    tyTerm t

tyPattern :: MonadInfer m => Pattern -> m Type
tyPattern (VarPat v) = do
    tv <- freshTyVar Star
    addType (v :>: toScheme tv)
    return tv
tyPattern (LitPat lit) = return (tyLit lit)
tyPattern (Pat con pts) = do
    conTM <- lookupTypes con
    case conTM of
        Nothing -> throwError (UnboundConstructor con)
        Just sc -> do
            ts <- mapM tyPattern pts
            tv <- freshTyVar Star
            ty <- freshen sc
            unify ty (foldr (-->) tv ts)
            applySubst tv

tyLit :: Literal -> Type
tyLit (IntLit _) = intCon
tyLit (RealLit _) = realCon

getTSKinds :: MonadError TypeError m => TypeSig -> m [(Id, Kind)]
getTSKinds t =
    forM vks $ \l ->
        case l of
            ((v, k1) : _) -> case filter (/= k1) (map snd l) of
                []       -> return (v, k1)
                (k2 : _) -> throwError $ MismatchingKinds v k1 k2
            _ -> error "TI.TypeInfer.getTSKinds: empty list, something went wrong"
  where
    vks :: [[(Id, Kind)]]
    vks =  groupBy ((==) `on` fst) $ sortBy (comparing fst) (go Star t)

    go :: Kind -> TypeSig -> [(Id, Kind)]
    go k (TyVar v) = [(v, k)]
    go k (TyApp l r) = go (Star :*> k) l ++ go Star r
    go _ _ = []

tyDataDecl :: MonadInfer m => Id -> [Id] -> DataBody -> m ()
tyDataDecl con vars body = do
    let res = foldl TyApp (TyCon con) $ map TyVar vars
    (ks, tss) <- tyDataBody res vars [] [] body
    let ks' = flip map vars $ \v -> case lookup v ks of
                  Nothing -> Star
                  Just k' -> k'
        k = foldl (:*>) Star ks'
    ksCtx <- getKinds
    putKinds ((con :>: k) : ksCtx)

    forM_ tss $ \(c, ts) -> do
        ty <- tyToTs ks ts
        ctx <- getTypes
        putTypes ((c :>: quantify (fv ty) ty) : ctx)

tyToTs :: MonadInfer m => [(Id, Kind)] -> TypeSig -> m Type
tyToTs _ (TyCon c) = do
    ksCtx <- getKinds
    case lookupAss c ksCtx of
        Nothing -> throwError $ UnboundTypeConstructor c
        Just k  -> return $ TyCon (c, k)
tyToTs ks (TyVar v) = case lookup v ks of
    Nothing -> error "TypeInfer.tyDataBody: something went wrong, var lookup"
    Just k  -> return $ TyVar (v, k)
tyToTs ks (TyApp l r) = liftM2 TyApp (tyToTs ks l) (tyToTs ks r)
tyToTs _ (TyGen i) = return $ TyGen i

tyDataBody :: MonadInfer m
              => TypeSig -> [Id] -> [(Id, Kind)] -> [(Id, TypeSig)]
              -> DataBody -> m ([(Id, Kind)], [(Id, TypeSig)])
tyDataBody _ _ ks tss [] = return (ks, tss)
tyDataBody res vars ks tss' ((con, tss) : body) = do
    let fn l = TyApp (TyApp (TyCon "(->)") l)
        ts = let x = foldr fn res tss in trace (show x) x
    ks' <- getTSKinds ts
    forM_ ks' $ \(v, k) -> case lookup v ks of
        Nothing -> if v `elem` vars then return ()
                   else throwError $ UnboundTypeVar v
        Just k' -> if k == k' then return ()
                   else throwError $ MismatchingKinds v k k'
    let ks'' = ks `union` ks'
    tyDataBody res vars ks'' ((con, ts) : tss') body
