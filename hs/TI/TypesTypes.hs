{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FunctionalDependencies,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module TI.TypesTypes
       -- ( Kind (..)
       -- , (-->)
       -- , HasKind (..)
       -- , Subst
       -- , Types (..)
       -- , (+->)
       -- , (@@)
       -- , TypeError (..)
       -- , mgu
       -- , varBind
       -- , Scheme (..)
       -- , toScheme
       -- , quantify
       -- , Instantiate (..)
       -- , Assump
       -- , lookupVar
       -- , lookupCon
       -- , lookupTyVar
       -- , lookupTyCon
       -- , MonadInfer (..)
       -- , TyVar
       -- , TyCon
       -- )
       where

import Control.Monad.Error
import Data.List (union, nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Fresh
import Syntax

-------------------------------------------------------------------------------

type Subst a = [(Var, a)]

infixr 3 -->
(-->) :: Type -> Type -> Type
ty1 --> ty2 = TyApp (TyApp (TyCon (con "(->)")) ty1) ty2

class SubstApply ty a | a -> ty where
    apply :: Subst ty -> a -> a

class HasVars ty where
    fv :: ty -> [Var]

instance SubstApply Type Type where
    apply sub (TyVar tyv) = case lookup tyv sub of
        Just ty -> ty
        Nothing -> TyVar tyv
    apply sub (TyApp ty1 ty2) = TyApp (apply sub ty1) (apply sub ty2)
    apply _ ty = ty

instance HasVars Type where
    fv (TyVar tyv) = [tyv]
    fv (TyApp ty1 ty2) = fv ty1 `union` fv ty2
    fv _ = []

instance SubstApply ty a => SubstApply ty [a] where
    apply sub = map (apply sub)

instance HasVars ty => HasVars [ty] where
    fv = nub . concatMap fv

(+->) :: SubstApply a a => Var -> a -> Subst a
tyv +-> ty = [(tyv, ty)]

infixr 4 @@
(@@) :: SubstApply ty ty => Subst ty -> Subst ty -> Subst ty
sub1 @@ sub2 = [(tyv, apply sub1 ty) | (tyv, ty) <- sub2] ++ sub1

-------------------------------------------------------------------------------

type Assump a = Map Id a

instance SubstApply ty a => SubstApply ty (Assump a) where
    apply sub = Map.map (apply sub)

instance HasVars ty => HasVars (Assump ty) where
    fv = fv . map snd . Map.toList

-------------------------------------------------------------------------------

data Scheme = Forall [Kind] Type
            deriving (Eq, Show)

toScheme :: Type -> Scheme
toScheme = Forall []

instance SubstApply Type Scheme where
    apply sub (Forall ks ty) = Forall ks $ apply sub ty

instance HasVars Scheme where
    fv (Forall _ ty) = fv ty

quantify :: MonadInfer m => [Var] -> Type -> m Scheme
quantify tyvs ty = do
    ks <- mapM kind $ map TyVar tyvs'
    return $ Forall ks (apply sub ty)
  where
    tyvs' = filter (`elem` fv ty) tyvs
    sub = zip tyvs' (map TyGen [0..])

class Instantiate ty where
    inst :: [Type] -> ty -> ty

instance Instantiate Type where
    inst tys (TyGen i) | length tys > i = tys !! i
                       | otherwise = error "TypesTypes.inst: TyGen out of bounds"
    inst tys (TyApp ty1 ty2) = TyApp (inst tys ty1) (inst tys ty2)
    inst _ ty = ty

-------------------------------------------------------------------------------

class (MonadFresh Integer m, MonadError TypeError m) => MonadInfer m where
    getTypes   :: m (Assump Scheme)
    putTypes   :: (Assump Scheme) -> m ()

    getKinds   :: m (Assump Kind)
    putKinds   :: (Assump Kind) -> m ()

    freshVar   :: m Var

data TypeError = TypeError String
               | UnboundVar Var
               | UnboundConstructor Con
               | MismatchingKinds Id Kind Kind
               | UnboundTypeVar Var
               | UnboundTypeConstructor Con
               | OccursCheck Type Type
               | KindOccursCheck Kind Kind
               deriving (Eq)

instance Error TypeError where
    strMsg = TypeError

lookupInfer :: MonadInfer m => m (Assump a) -> (Id -> TypeError) -> Id -> m a
lookupInfer m f v = do
   xM <- liftM (Map.lookup v) m
   case xM of
       Nothing -> throwError $ f v
       Just x  -> return x

addType :: MonadInfer m => Id -> Scheme -> m ()
addType i ty = do
    tyCtx <- getTypes
    putTypes (Map.insert i ty tyCtx)

addKind :: MonadInfer m => Id -> Kind -> m ()
addKind i k = do
    kCtx <- getKinds
    putKinds (Map.insert i k kCtx)

lookupVar :: MonadInfer m => Var -> m Scheme
lookupVar = lookupInfer getTypes (UnboundVar . var) . unVar

addVar :: MonadInfer m => Var -> Scheme -> m ()
addVar = addType . unVar

addCon :: MonadInfer m => Con -> Scheme -> m ()
addCon = addType . unCon

lookupCon :: MonadInfer m => Con -> m Scheme
lookupCon = lookupInfer getTypes (UnboundConstructor . con) . unCon

lookupTyVar :: MonadInfer m => Var -> m Kind
lookupTyVar = lookupInfer getKinds (UnboundTypeVar . var) . unVar

addTyVar :: MonadInfer m => Var -> Kind -> m ()
addTyVar tyv = addKind (unVar tyv)

addTyCon :: MonadInfer m => Con -> Kind -> m ()
addTyCon = addKind . unCon

lookupTyCon :: MonadInfer m => Con -> m Kind
lookupTyCon = lookupInfer getKinds (UnboundTypeConstructor . con) . unCon

-------------------------------------------------------------------------------

class SubstApply a a => Unifiable a where
    mgu :: MonadInfer m => a -> a -> m (Subst a)

instance Unifiable Type where
    mgu (TyApp tyl1 tyr1) (TyApp tyl2 tyr2) = do
        sub1 <- mgu tyl1 tyl2
        sub2 <- mgu (apply sub1 tyr1) (apply sub1 tyr2)
        return (sub2 @@ sub1)
    mgu (TyVar tyv) ty = varBind tyv ty
    mgu ty (TyVar tyv) = varBind tyv ty
    mgu (TyCon tyc1) (TyCon tyc2) | tyc1 == tyc2 = return []
    mgu _  _ = throwError (strMsg "Types do not unify")

varBind :: MonadInfer m => Var -> Type -> m (Subst Type)
varBind tyv ty
    | TyVar tyv == ty = return []
    | tyv `elem` fv ty = throwError $ OccursCheck (TyVar tyv) ty
    | otherwise = do
        tyvk <- kind (TyVar tyv)
        tyk <- kind ty
        if tyvk /= tyk then
            throwError (strMsg "Different kinds")
          else return (tyv +-> ty)

-------------------------------------------------------------------------------

infixr 3 :*>
data Kind = Star
          | Kind :*> Kind
          | KVar Var
          deriving (Eq, Show)

class HasKind ty where
    kind :: MonadInfer m => ty -> m Kind

instance HasKind Type where
    kind (TyVar tyv) = lookupTyVar tyv
    kind (TyCon tyc) = lookupTyCon tyc
    kind (TyApp ty _) = do
        k' <- kind ty
        case k' of
            _ :*> k -> return k
            _       -> error "TypesTypes.kind: malformed type (mismatching kind)"
    kind _ = error "TypesTypes.kind: TyGen"

instance HasKind Scheme where
    kind (Forall _ ty) = kind ty

instance SubstApply Kind Kind where
    apply sub (KVar v) = case lookup v sub of
        Nothing -> KVar v
        Just k  -> k
    apply sub (k1 :*> k2) = apply sub k1 :*> apply sub k2
    apply _ k = k

instance HasVars Kind where
    fv (KVar kv) = [kv]
    fv (k1 :*> k2) = fv k1 `union` fv k2
    fv _ = []

instance Unifiable Kind where
    mgu (kl1 :*> kr1) (kl2 :*> kr2) = do
        sub1 <- mgu kl1 kl2
        sub2 <- mgu (apply sub1 kr1) (apply sub1 kr2)
        return (sub2 @@ sub1)
    mgu (KVar kv) k = kVarBind kv k
    mgu k (KVar kv) = kVarBind kv k
    mgu Star Star = return []
    mgu _ _ = throwError $ strMsg "Kinds do not unify"

kVarBind :: MonadInfer m => Var -> Kind -> m (Subst Kind)
kVarBind kv k
  | KVar kv == k = return []
  | kv `elem` fv k = throwError $ KindOccursCheck (KVar kv) k
  | otherwise = return (kv +-> k)
