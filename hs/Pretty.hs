{-# LANGUAGE OverloadedStrings #-}
module Pretty
       ( prettyML
       , prettyDesugar
       , pTypeS
       , pTypeSig
       , pParensTypeS

       , prettyScheme
       , prettyType
       , prettyAssumps
       , prettySubst
       , pType
       , pScheme
       , pKind
       ) where

import Text.PrettyPrint

import Syntax
import TI.TypesTypes

-------------------------------------------------------------------------------

prettyML :: [Decl FullTerm] -> String
prettyML = render . vcat . map (pDecl (hsep . (map pPattern)) pPattern)

prettyDesugar :: [Decl DTerm] -> String
prettyDesugar = render . vcat . map (pDecl text text)

pTerm :: (fn -> Doc) -> (lt -> Doc) -> Term fn lt -> Doc
pTerm _ _ (Var v) = text v
pTerm _ _ (Con c) = text c
pTerm f l (Abs pts t) = "\\" <> f pts <+> "->" <+> pTerm f l t
pTerm f l (App t1 t2) = pTerm f l t1 <+> parensTerm f l t2
pTerm f l (Let pt t1 t2) = sep [ "let" <+> l pt <+> equals <+> pTerm f l t1 <+> "in"
                               , pTerm f l t2
                               ]
pTerm _ _ (Literal lit) = pLiteral lit
pTerm f l (Case t cases) = ("case" <+> pTerm f l t <+> "of") $+$
                           nest 4 (pCases (pTerm f l) cases)

parensTerm :: (fn -> Doc) -> (lt -> Doc) -> Term fn lt -> Doc
parensTerm f l t = case t of
    Abs _ _ -> parens d
    Let _ _ _ -> parens d
    App _ _ -> parens d
    _ -> d
  where
    d = pTerm f l t

pPattern :: Pattern -> Doc
pPattern (VarPat v) = text v
pPattern (Pat c pts) = parens (text c <+> hsep (map pPattern pts))
pPattern (LitPat lit) = pLiteral lit

pLiteral :: Literal -> Doc
pLiteral (IntLit i) = text i
pLiteral (RealLit r) = text r

pCases :: (a -> Doc) -> [(Pattern, a)] -> Doc
pCases tf (case' : cases) = (space <+> p case') $$
                            vcat (map (\case'' -> "|" <+> p case'') cases)
  where
    p (pt, t) = pPattern pt <+> "->" <+> tf t
pCases _ _ = "Parser.pCases: Received 0 cases"

pDecl :: (fn -> Doc) -> (lt -> Doc) -> Decl (Term fn lt) -> Doc
pDecl f l (ValDecl v t) = sep ["let" <+> text v <+> equals, nest 4 (pTerm f l t)]
pDecl _ _ (DataDecl c tyvs dbody)
    = "data" <+> text c <+> hsep (map text tyvs) <+> "where" $$
      nest 4 (pDataBody dbody)

pDataBody :: DataBody -> Doc
pDataBody (opt : opts) = space <+> p opt $$ vcat (map (\opt' -> "|" <+> p opt') opts)
  where
    p (c, tss) = text c <+> hsep (map pTypeSig tss)
pDataBody _ = "Parser.pDataBody: Received 0 options"

pTypeSig :: TypeSig -> Doc
pTypeSig (TyApp (TyApp (TyCon "(->)") ts1) ts2) =
    pParensTypeS pTypeSig ts1 <+> "->" <+> pTypeSig ts2
pTypeSig (TyApp (TyApp (TyCon "(,)") ts1) ts2) =
    "(" <> pTypeSig ts1 <> "," <+> pTypeSig ts2 <> ")"
pTypeSig (TyApp (TyApp (TyApp (TyCon "(,,)") ts1) ts2) ts3) =
    "(" <> pTypeSig ts1 <> "," <+> pTypeSig ts2 <> "," <+> pTypeSig ts3 <> ")"
pTypeSig ts = pTypeS text ts

pTypeS :: (t -> Doc) -> TypeS t -> Doc
pTypeS f (TyVar tyv) = f tyv
pTypeS f (TyCon tyc) = f tyc
pTypeS f (TyApp ty1 ty2) = pParensTypeS (pTypeS f) ty1 <+> pTypeS f ty2
pTypeS _ (TyGen i) = text (show i)

pParensTypeS :: (TypeS t -> Doc) -> TypeS t -> Doc
pParensTypeS f ty = case ty of
    TyApp _ _ -> parens d
    _ -> d
  where
    d = f ty

-------------------------------------------------------------------------------

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
