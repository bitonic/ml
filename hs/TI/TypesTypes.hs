{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings,
             TypeSynonymInstances #-}
module TI.TypesTypes
       ( Kind (..)
       , Type
       , TyVar
       , TyCon
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
       , Assump (..)
       , lookupAss
       ) where

import Control.Monad.Error
import Data.List (union, nub)

import Syntax

-------------------------------------------------------------------------------

infixr 3 :*>
data Kind = Star
          | Kind :*> Kind
          deriving (Eq, Show)

type Type = TypeS (Id, Kind)

type TyVar = (Id, Kind)
type TyCon = (Id, Kind)

infixr 3 -->
(-->) :: Type -> Type -> Type
ty1 --> ty2 = TyApp (TyApp (TyCon ("(->)", Star :*> Star :*> Star)) ty1) ty2

class HasKind ty where
    kind :: ty -> Kind

instance HasKind (Id, Kind) where
    kind = snd

instance HasKind Type where
    kind (TyVar tyv) = kind tyv
    kind (TyCon tyc) = kind tyc
    kind (TyApp ty _) = case kind ty of
        _ :*> k -> k
        _        -> error "TypesTypes.kind: malformed type (mismatching kind)"
    kind _ = error "TypesTypes.kind: TyGen"

type Subst = [(TyVar, Type)]

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

mgu :: MonadError TypeError m => Type -> Type -> m Subst
mgu (TyApp tyl1 tyr1) (TyApp tyl2 tyr2) = do
    sub1 <- mgu tyl1 tyl2
    sub2 <- mgu (apply sub1 tyr1) (apply sub1 tyr2)
    return (sub2 @@ sub1)
mgu (TyVar tyv) ty = varBind tyv ty
mgu ty (TyVar tyv) = varBind tyv ty
mgu (TyCon tyc1) (TyCon tyc2) | tyc1 == tyc2 = return []
mgu _  _ = throwError (strMsg "Types do not unify")

varBind :: MonadError TypeError m => TyVar -> Type -> m Subst
varBind tyv ty
    | TyVar tyv == ty = return []
    | tyv `elem` fv ty = throwError $ OccursCheck (TyVar tyv) ty
    | kind tyv /= kind ty = throwError (strMsg "Different kinds")
    | otherwise = return (tyv +-> ty)

data Scheme = Forall [Kind] Type
            deriving (Eq, Show)

instance Types Scheme where
    apply sub (Forall ks ty) = Forall ks (apply sub ty)

    fv (Forall _ ty) = fv ty

toScheme :: Type -> Scheme
toScheme = Forall []

quantify :: [TyVar] -> Type -> Scheme
quantify tyvs ty = Forall ks (apply sub ty)
  where
    tyvs' = filter (`elem` fv ty) tyvs
    sub = zip tyvs' (map TyGen [0..])
    ks = map kind tyvs'

class Instantiate ty where
    inst :: [Type] -> ty -> ty

instance Instantiate Type where
    inst tys (TyGen i) | length tys > i = tys !! i
                       | otherwise = error "TypesTypes.inst: TyGen out of bounds"
    inst tys (TyApp ty1 ty2) = TyApp (inst tys ty1) (inst tys ty2)
    inst _ ty = ty

data Assump a = Id :>: a
              deriving (Eq, Show)

instance Types ty => Types (Assump ty) where
    apply sub (tyv :>: sc) = tyv :>: apply sub sc
    fv (_ :>: sc) = fv sc

lookupAss :: Id -> [Assump b] -> Maybe b
lookupAss v = lookup v . map (\(v' :>: sc) -> (v', sc))
