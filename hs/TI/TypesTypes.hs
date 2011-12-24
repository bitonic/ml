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
         -- * Pretty printing
       , prettyScheme
       , prettyType
       , prettyAssumps
       , prettySubst
       , pType
       , pScheme
       , pKind
       ) where

import Control.Monad.Error
import Data.List (union, nub)
import Text.PrettyPrint

import Syntax

import Debug.Trace

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

instance Show TypeError where
    show (TypeError s) = "TypeError: " ++ s
    show (UnboundVar v) = "Unboud variable \"" ++ v ++ "\""
    show (UnboundConstructor c) = "Unbound constructor \"" ++ c ++ "\""
    show (MismatchingKinds tyv k1 k2) =
        "Mismatching kinds for type variable " ++ tyv ++ ": \"" ++
        prettyKind k1 ++ "\" and \"" ++ prettyKind k2 ++ "\""
    show (UnboundTypeVar tyv) = "Unbound type variable \"" ++ tyv ++ "\""
    show (UnboundTypeConstructor tyc) =
        "Unbound type constructor \"" ++ tyc ++ "\""
    show (OccursCheck ty1 ty2) =
         "Occurs check fails when unifying \"" ++ prettyType ty1 ++ "\" with \"" ++
         prettyType ty2 ++ "\""

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

-------- PRETTY PRINTING YO ---------------------------------------------------

prettyScheme :: Scheme -> String
prettyScheme = render . pScheme

prettyType :: Type -> String
prettyType = render . pType

pType :: Type -> Doc
pType (TyApp (TyApp (TyCon ("(->)", _)) ty1) ty2) =
    pParensTypeS pType ty1 <+> "->" <+> pType ty2
pType (TyApp (TyApp (TyCon ("(,)", _)) ty1) ty2) =
    "(" <> pType ty1 <> "," <+> pType ty2 <> ")"
pType (TyApp (TyApp (TyApp (TyCon ("(,,)", _)) ty1) ty2) ty3) =
    "(" <> pType ty1 <> "," <+> pType ty2 <> "," <+> pType ty3 <> ")"
pType ty = pTypeS (text . fst) ty

prettyAssumps :: (a -> Doc) -> [Assump a] -> String
prettyAssumps f = render . vcat . map (pAssump f)

pAssump :: (a -> Doc) -> Assump a -> Doc
pAssump f (x :>: ty) = text x <+> ":" <+> f ty

pScheme :: Scheme -> Doc
pScheme (Forall _ ty) = pType ty

prettyKind :: Kind -> String
prettyKind = render . pKind

pKind :: Kind -> Doc
pKind Star = "*"
pKind (k1 :*> k2) = p k1 <+> "->" <+> pKind k2
  where
    p Star = "*"
    p k = parens (pKind k)

prettySubst :: Subst -> String
prettySubst = render . vcat . map (\(tyv, ty) -> text (fst tyv) <+> "=>" <+> pType ty)