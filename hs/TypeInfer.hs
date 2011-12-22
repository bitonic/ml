{-# LANGUAGE FlexibleInstances, FlexibleContexts, TupleSections #-}
module TypeInfer where

import Control.Arrow (first)
import Control.Monad.Error
import Control.Monad.State
import Data.List ((\\))

import Desugar
import TypesTypes
import Lexer (Id)
import Fresh

inferType :: [Decl DTerm] -> Either TypeError [(Id, Scheme)]
inferType decls = undefined

inferTerm :: DTerm -> Either TypeError Type
inferTerm t = evalFresh (runErrorT (evalStateT (tyTerm t) (([], []) :: InferState))) (0 :: Integer)

class (MonadFresh Integer m, MonadError TypeError m) => InferMonad m where
    applySubst :: Types t => t -> m t
    extSubst   :: Subst -> m ()

    getCtx :: m [Assump]
    putCtx :: [Assump] -> m ()

    unify      :: Type -> Type -> m ()


type InferState = (Subst, [Assump])

instance InferMonad (StateT InferState (ErrorT TypeError (Fresh Integer))) where
    applySubst t = liftM (`apply` t) (gets fst)
    extSubst s = modify (first (s @@))

    getCtx = gets snd
    putCtx ctx = modify (\(s, _) -> (s, ctx))

    unify t1 t2 = do s1 <- gets fst
                     s2 <- mgu (apply s1 t1) (apply s1 t2)
                     extSubst s2

lookupCtx :: InferMonad m => Id -> m (Maybe Scheme)
lookupCtx v = liftM (lookupAss v) getCtx

addCtx :: InferMonad m => Assump -> m ()
addCtx ass = do {ctx <- getCtx; putCtx (ass : ctx)}

updateCtx :: InferMonad m => m ()
updateCtx = do {ctx <- getCtx; putCtx =<< applySubst ctx}

freshTyVar :: InferMonad m => Kind -> m Type
freshTyVar k = liftM (\i -> TyVar ("_v" ++ show i, k)) fresh

freshen :: InferMonad m => Scheme -> m Type
freshen (Forall ks t) = liftM (`inst` t) (mapM freshTyVar ks)

intCon, realCon :: Type
intCon = TyCon ("Int", Star)
realCon = TyCon ("Real", Star)

tyTerm :: InferMonad m => DTerm -> m Type
tyTerm (Var v) = do
    tM <- lookupCtx v
    case tM of
        Just t -> freshen t
        Nothing -> throwError (UnboundVar v)
tyTerm (Con c) = do
    tM <- lookupCtx c
    case tM of
        Just t -> freshen t
        Nothing -> throwError (UnboundConstructor c)
tyTerm (Abs v t) = do
    tv <- freshTyVar Star
    addCtx (v :>: Forall [] tv)
    a <- tyTerm t
    applySubst (tv --> a)
tyTerm (App l r) = do
    tv <- freshTyVar Star
    a <- tyTerm l
    updateCtx
    b <- tyTerm r
    (`unify` (b --> tv)) =<< applySubst a
    applySubst tv
tyTerm (Let v t1 t2) = do
    a <- tyTerm t1
    updateCtx
    ctxFv <- liftM fv getCtx
    let a' = quantify (fv a \\ ctxFv) a
    addCtx (v :>: a')
    tyTerm t2
tyTerm (Fix v t) = do
    tv <- freshTyVar Star
    addCtx (v :>: Forall [] tv)
    a <- tyTerm t
    (`unify` a) =<< applySubst tv
    applySubst a
tyTerm (Literal l) = return (tyLit l)
tyTerm (Case t cases) = error "TypeInfer.tyTerm: case of not implemented"

tyLit :: Literal -> Type
tyLit (IntLit _) = intCon
tyLit (RealLit _) = realCon
