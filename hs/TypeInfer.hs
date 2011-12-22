{-# LANGUAGE FlexibleInstances, FlexibleContexts, TupleSections, ScopedTypeVariables #-}
module TypeInfer where

import Control.Monad.Error
import Control.Monad.State
import Data.List ((\\))

import Desugar
import TypesTypes
import Lexer (Id)
import Fresh

inferType :: [Decl DTerm] -> Either TypeError ([Assump Scheme], [Assump Kind])
inferType decls' = evalFresh (runErrorT (go baseTypes [] decls')) (0 :: Integer)
  where
    go ts ks [] = return (reverse ts, reverse ks)
    go ts ks (ValDecl v t : decls) = do
        ty <- evalStateT (tyTerm t) (InferState [] ts ks)
        go ((v :>: quantify (fv ty) ty) : ts) ks decls

baseTypes :: [Assump Scheme]
baseTypes =
  [ "(,)" :>:
    ( Forall [Star, Star]
      (TyGen 0 --> TyGen 1 -->
       TyApp (TyApp (TyCon ("(,)", Star :*> Star :*> Star)) (TyGen 0)) (TyGen 1)
    ))
  , "(,,)" :>:
    ( Forall [Star, Star, Star]
      (TyGen 0 --> TyGen 1 --> TyGen 2 -->
       TyApp (TyApp (TyApp (TyCon ("(,,)", Star :*> Star :*> Star :*> Star)) (TyGen 0)) (TyGen 1)) (TyGen 2)
    ))
  ]

class (MonadFresh Integer m, MonadError TypeError m) => InferMonad m where
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

instance InferMonad (StateT InferState (ErrorT TypeError (Fresh Integer))) where
    applySubst t = liftM (`apply` t) (gets substitution)
    extSubst s = modify (\(InferState s' ts ks) -> (InferState (s @@ s') ts ks))

    getTypes = gets types
    putTypes ts = modify (\(InferState s _ ks) -> (InferState s ts ks))

    getKinds = gets kinds
    putKinds ks = modify (\(InferState s ts _) -> (InferState s ts ks))

    unify t1 t2 = do s1 <- gets substitution
                     s2 <- mgu (apply s1 t1) (apply s1 t2)
                     extSubst s2

-- runInferMonad :: InferMonad m => m a -> InferState -> Either TypeError a
runInferMonad m s = evalFresh (runErrorT (evalStateT m s)) (0 :: Integer)

lookupTypes :: InferMonad m => Id -> m (Maybe Scheme)
lookupTypes v = liftM (lookupAss v) getTypes

addType :: InferMonad m => Assump Scheme -> m ()
addType ass = do {ctx <- getTypes; putTypes (ass : ctx)}

updateTypes :: InferMonad m => m ()
updateTypes = do {ctx <- getTypes; putTypes =<< applySubst ctx}

freshTyVar :: (Show c, MonadFresh c m) => Kind -> m Type
freshTyVar k = liftM (\i -> TyVar ("_v" ++ show i, k)) fresh

freshen :: (Show c, MonadFresh c m) => Scheme -> m Type
freshen (Forall ks t) = liftM (`inst` t) (mapM freshTyVar ks)

intCon, realCon :: Type
intCon = TyCon ("Int", Star)
realCon = TyCon ("Real", Star)

tyTerm :: InferMonad m => DTerm -> m Type
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

tyCases :: InferMonad m => Type -> [(Pattern, DTerm)] -> m Type
tyCases ty [] = return ty
tyCases ty (cs : css) = do
    ty' <- tyCase ty cs
    unify ty ty'
    ty'' <- applySubst ty'
    tyCases ty'' css

tyCase :: InferMonad m => Type -> (Pattern, DTerm) -> m Type
tyCase ty (pt, t) = do
    typt <- tyPattern pt
    unify ty typt
    tyTerm t

tyPattern :: InferMonad m => Pattern -> m Type
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
