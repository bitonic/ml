module TypesTypes where

import Lexer (Id)

data Kind = Star | KArr Kind Kind
          deriving (Eq, Show)

data Type = TyVar TyVar
          | TyCon TyCon
          | TyApp Type Type
          | TyGen Int

type TyVar = (Id, Kind)
type TyCon = (Id, Kind)

class HasKind t where
    kind :: t -> Kind

instance HasKind (Id, Kind) where
    kind = snd

instance HasKind Type where
    TyVar tv = kind tv
    TyCon tc = kind tc
    TyApp t _ = case kind t of
        KArr _ k -> k
        _        -> error "TypesTypes.kind: malformed type (mismatching kind)"
    _ = error "TypesTypes.kind: TyGen"

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
(@@) :: Subst -> Substs -> Subst
s1 @@ s2 = [(tv, apply s1 t) | (tv, t) <- s2] ++ s1

mgu :: Monad m => Type -> Type -> m Subst
mgu (TyApp l1 r1) (TyApp l2 r2) = do s1 <- mgu l1 l2
                                     s2 <- mgu (apply s1 r1) (apply s1 r2)
                                     return (s2 @@ s1)
mgu (TyVar tv) t = varBind tv t
mgu t (TyVar tv) = varBind tv t
mgu (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return []
mgu _  _ = fail "Types do not unify"

varBind :: Monad m => TyVar -> Type -> m Subst
varBind tv t | TyVar tv == t = []
             | tv `elem` fv t = fail "Occurs check fails"
             | kind tv /= kind t = fail "Different kinds"
             | otherwise = return (tv +-> t)

data Scheme = Forall [Kind] Type

instance Types Scheme where
    apply s (Forall ks t) = Forall ks (apply s t)

    tv (Forall ks t) = fv t

quantify :: [TyVar] -> Type -> Scheme
quantify tvs ty = Forall ks (apply s ty)
  where
    tvs' = filter (`elem` fv tvs) tvs
    s = zip tvs' (map TyGen [0..])
    ks = map kind tvs'
