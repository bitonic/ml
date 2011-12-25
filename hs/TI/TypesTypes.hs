{-# LANGUAGE FlexibleContexts, TypeSynonymInstances #-}
module TI.TypesTypes
       ( Kind (..)
       , (-->)
       , HasKind (..)
       , Subst
       , Types (..)
       , (+->)
       , (@@)
       , TypeError (..)
       , mgu
       , varBind
       , Scheme (..)
       , toScheme
       , quantify
       , Instantiate (..)
       , Assump
       , lookupVar
       , lookupCon
       , lookupTyVar
       , lookupTyCon
       , MonadInfer (..)
       , TyVar
       , TyCon
       ) where

import Control.Monad.Error
import Data.List (union, nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Fresh
import Syntax

-------------------------------------------------------------------------------

type TyVar = Id
type TyCon = Id

type Subst = [(TyVar, Type)]

infixr 3 -->
(-->) :: Type -> Type -> Type
ty1 --> ty2 = TyApp (TyApp (TyCon "(->)") ty1) ty2

class Types ty where
    apply :: Subst -> ty -> ty
    fv    :: ty -> [TyVar]

instance Types Type where
    apply sub (TyVar tyv) = case lookup tyv sub of
        Just ty -> ty
        Nothing -> TyVar tyv
    apply sub (TyApp ty1 ty2) = TyApp (apply sub ty1) (apply sub ty2)
    apply _ ty = ty

    fv (TyVar tyv) = [tyv]
    fv (TyApp ty1 ty2) = fv ty1 `union` fv ty2
    fv _ = []

instance Types ty => Types [ty] where
    apply sub = map (apply sub)

    fv = nub . concatMap fv

(+->) :: TyVar -> Type -> Subst
tyv +-> ty = [(tyv, ty)]

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
sub1 @@ sub2 = [(tyv, apply sub1 ty) | (tyv, ty) <- sub2] ++ sub1

-------------------------------------------------------------------------------

type Assump a = Map Id a

instance Types ty => Types (Assump ty) where
    apply sub = Map.map (apply sub)

    fv = fv . map snd . Map.toList

-------------------------------------------------------------------------------

data Scheme = Forall [Kind] Type
            deriving (Eq, Show)

toScheme :: Type -> Scheme
toScheme = Forall []

instance Types Scheme where
    apply sub (Forall ks ty) = Forall ks $ apply sub ty

    fv (Forall _ ty) = fv ty

quantify :: MonadInfer m => [Id] -> Type -> m Scheme
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

    unify      :: Type -> Type -> m ()

data TypeError = TypeError String
               | UnboundVar Id
               | UnboundConstructor Id
               | MismatchingKinds Id Kind Kind
               | UnboundTypeVar Id
               | UnboundTypeConstructor Id
               | OccursCheck Type Type
               deriving (Eq)

instance Error TypeError where
    strMsg = TypeError

lookupInfer :: MonadInfer m => m (Assump a) -> (Id -> TypeError) -> Id -> m a
lookupInfer m f v = do
   xM <- liftM (Map.lookup v) m
   case xM of
       Nothing -> throwError $ f v
       Just x  -> return x

lookupVar :: MonadInfer m => Id -> m Scheme
lookupVar = lookupInfer getTypes UnboundVar

lookupCon :: MonadInfer m => Id -> m Scheme
lookupCon = lookupInfer getTypes UnboundConstructor

lookupTyVar :: MonadInfer m => Id -> m Kind
lookupTyVar = lookupInfer getKinds UnboundTypeVar

lookupTyCon :: MonadInfer m => Id -> m Kind
lookupTyCon = lookupInfer getKinds UnboundTypeConstructor

-------------------------------------------------------------------------------

infixr 3 :*>
data Kind = Star
          | Kind :*> Kind
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

mgu :: MonadInfer m => Type -> Type -> m Subst
mgu (TyApp tyl1 tyr1) (TyApp tyl2 tyr2) = do
    sub1 <- mgu tyl1 tyl2
    sub2 <- mgu (apply sub1 tyr1) (apply sub1 tyr2)
    return (sub2 @@ sub1)
mgu (TyVar tyv) ty = varBind tyv ty
mgu ty (TyVar tyv) = varBind tyv ty
mgu (TyCon tyc1) (TyCon tyc2) | tyc1 == tyc2 = return []
mgu _  _ = throwError (strMsg "Types do not unify")

varBind :: MonadInfer m => TyVar -> Type -> m Subst
varBind tyv ty
    | TyVar tyv == ty = return []
    | tyv `elem` fv ty = throwError $ OccursCheck (TyVar tyv) ty
    | otherwise = do
        tyvk <- kind (TyVar tyv)
        tyk <- kind ty
        if tyvk /= tyk then
            throwError (strMsg "Different kinds")
          else return (tyv +-> ty)
