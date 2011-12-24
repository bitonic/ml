{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings,
             TypeSynonymInstances #-}
module TI.TypesTypes
       ( Kind (..)
       , Type
       , TypeS (..)
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
       , pType
       , pScheme
       ) where

import Control.Monad.Error
import Data.List (union, nub)
import Text.PrettyPrint

import Syntax

infixr 3 :*>
data Kind = Star
          | Kind :*> Kind
          deriving (Eq, Show)

type Type = TypeS (Id, Kind)

type TyVar = (Id, Kind)
type TyCon = (Id, Kind)

infixr 3 -->
(-->) :: Type -> Type -> Type
l --> r = TyApp (TyApp (TyCon ("(->)", Star :*> Star :*> Star)) l) r

class HasKind t where
    kind :: t -> Kind

instance HasKind (Id, Kind) where
    kind = snd

instance HasKind Type where
    kind (TyVar tv) = kind tv
    kind (TyCon tc) = kind tc
    kind (TyApp t _) = case kind t of
        _ :*> k -> k
        _        -> error "TypesTypes.kind: malformed type (mismatching kind)"
    kind _ = error "TypesTypes.kind: TyGen"

type Subst = [(TyVar, Type)]

class Types t where
    apply :: Subst -> t -> t
    fv    :: t -> [TyVar]

instance Types Type where
    apply s (TyVar tv) = case lookup tv s of
        Just t  -> t
        Nothing -> TyVar tv
    apply s (TyApp l r) = TyApp (apply s l) (apply s r)
    apply _ t = t

    fv (TyVar tv) = [tv]
    fv (TyApp l r) = fv l `union` fv r
    fv _ = []

instance Types a => Types [a] where
    apply s = map (apply s)

    fv = nub . concatMap fv

(+->) :: TyVar -> Type -> Subst
tv +-> t = [(tv, t)]

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(tv, apply s1 t) | (tv, t) <- s2] ++ s1

data TypeError = TypeError String
               | UnboundVar Id
               | UnboundConstructor Id
               | MismatchingKinds Id Kind Kind
               | UnboundTypeVar Id
               | UnboundTypeConstructor Id
               deriving (Show, Eq)

instance Error TypeError where
    strMsg = TypeError

mgu :: MonadError TypeError m => Type -> Type -> m Subst
mgu (TyApp l1 r1) (TyApp l2 r2) = do s1 <- mgu l1 l2
                                     s2 <- mgu (apply s1 r1) (apply s1 r2)
                                     return (s2 @@ s1)
mgu (TyVar tv) t = varBind tv t
mgu t (TyVar tv) = varBind tv t
mgu (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return []
mgu _  _ = throwError (strMsg "Types do not unify")

varBind :: MonadError TypeError m => TyVar -> Type -> m Subst
varBind tv t | TyVar tv == t = return []
             | tv `elem` fv t = throwError (strMsg "Occurs check fails")
             | kind tv /= kind t = throwError (strMsg "Different kinds")
             | otherwise = return (tv +-> t)

data Scheme = Forall [Kind] Type
            deriving (Eq, Show)

instance Types Scheme where
    apply s (Forall ks t) = Forall ks (apply s t)

    fv (Forall _ t) = fv t

toScheme :: Type -> Scheme
toScheme = Forall []

quantify :: [TyVar] -> Type -> Scheme
quantify tvs ty = Forall ks (apply s ty)
  where
    tvs' = filter (`elem` fv ty) tvs
    s = zip tvs' (map TyGen [0..])
    ks = map kind tvs'

class Instantiate t where
    inst :: [Type] -> t -> t

instance Instantiate Type where
    inst ts (TyGen i) | length ts > i = ts !! i
                      | otherwise = error "TypesTypes.inst: TyGen out of bounds"
    inst ts (TyApp l r) = TyApp (inst ts l) (inst ts r)
    inst _ t = t

data Assump a = Id :>: a
              deriving (Eq, Show)

instance Types t => Types (Assump t) where
    apply s (v :>: sc) = v :>: apply s sc
    fv (_ :>: sc) = fv sc

lookupAss :: Id -> [Assump b] -> Maybe b
lookupAss v = lookup v . map (\(v' :>: sc) -> (v', sc))

-------- PRETTY PRINTING YO ---------------------------------------------------

prettyScheme :: Scheme -> String
prettyScheme = render . pScheme

prettyType :: Type -> String
prettyType = render . p
  where
    p (TyApp (TyApp (TyCon ("(->)", _)) l) r) =
        parensType p l <+> "->" <+> p r
    p (TyApp (TyApp (TyCon ("(,)", _)) l) r) =
        "(" <> p l <> "," <+> p r <> ")"
    p (TyApp (TyApp (TyApp (TyCon ("(,,)", _)) l) m) r) =
        "(" <> p l <> "," <+> p m <> "," <+> p r <> ")"
    p t = pType (text . fst) t

prettyAssumps :: (a -> Doc) -> [Assump a] -> String
prettyAssumps f = render . vcat . map (pAssump f)

pAssump :: (a -> Doc) -> Assump a -> Doc
pAssump f (x :>: ty) = text x <+> ":" <+> f ty

pScheme :: Scheme -> Doc
pScheme (Forall _ ty) = pType (text . fst) ty