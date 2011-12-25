{-# LANGUAGE OverloadedStrings #-}
module Pretty
       ( prettyML
       , prettyDesugar
       , pTypeS
       , pType
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
    p (c, tss) = text c <+> hsep (map pType tss)
pDataBody _ = "Parser.pDataBody: Received 0 options"

pType :: Type -> Doc
pType (TyApp (TyApp (TyCon "(->)") ts1) ts2) =
    pParensTypeS pType ts1 <+> "->" <+> pType ts2
pType (TyApp (TyApp (TyCon "(,)") ts1) ts2) =
    "(" <> pType ts1 <> "," <+> pType ts2 <> ")"
pType (TyApp (TyApp (TyApp (TyCon "(,,)") ts1) ts2) ts3) =
    "(" <> pType ts1 <> "," <+> pType ts2 <> "," <+> pType ts3 <> ")"
pType (TyVar tyv) = text tyv
pType (TyCon tyc) = text tyc
pType (TyApp ty1 ty2) = pParensTypeS ty1 <+> pTypeS ty2
pType (TyGen i) = text (show i)

pParensType :: Type -> Doc
pParensType ty = case ty of
    TyApp _ _ -> parens d
    _ -> d
  where
    d = pType ty

-------------------------------------------------------------------------------

prettyScheme :: Scheme -> String
prettyScheme = render . pScheme

prettyType :: Type -> String
prettyType = render . pType

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
