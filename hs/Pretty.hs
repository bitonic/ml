{-# LANGUAGE OverloadedStrings #-}
module Pretty (prettyML) where

import Data.List (intersperse)
import Text.PrettyPrint

import Parser

prettyML :: [Decl] -> String
prettyML = render . vcat . map pDecl

pTerm :: Term -> Doc
pTerm (Var v) = text v
pTerm (Abs pts t) = "\\" <> hsep (map pPattern pts) <+> "->" <+> pTerm t
pTerm (App t1 t2) = parensTerm t1 <+> parensTerm t2
pTerm (Let pt t1 t2) = sep [ "let" <+> pPattern pt <+> equals <+> pTerm t1 <+> "in"
                           , pTerm t2
                           ]
pTerm (Fix f t) = "fix" <+> text f <+> "->" <+> pTerm t
pTerm (Literal lit) = pLiteral pTerm lit
pTerm (Case t cases) = ("case" <+> pTerm t <+> "of") $+$ nest 4 (pCases cases)

parensTerm :: Term -> Doc
parensTerm t = case t of
    Abs _ _   -> parens d
    Let _ _ _ -> parens d
    Fix _ _   -> parens d
    App _ _   -> parens d
    _         -> d
  where
    d = pTerm t

pPattern :: Pattern -> Doc
pPattern (VarPat v) = text v
pPattern (Pat con Nothing) = text con
pPattern (Pat con (Just pt)) = parens (text con <+> pPattern pt)
pPattern (LitPat lit) = pLiteral pPattern lit
pPattern (WildPat w) = text w

pLiteral :: (a -> Doc) -> Literal a -> Doc
pLiteral _ (IntLit i) = text (show i)
pLiteral _ (RealLit r) = text (show r)
pLiteral f (TupleLit xs) = parens . hcat . intersperse comma . map f $ xs

pCases :: [(Pattern, Term)] -> Doc
pCases (c : cs) = (space <+> p c) $$ vcat (map (\c' -> "|" <+> p c') cs)
  where
    p (pt, t) = pPattern pt <+> "->" <+> pTerm t
pCases _ = "Pretty.pCases: Received 0 cases"

pDecl :: Decl -> Doc
pDecl (ValDecl v t) = sep ["let" <+> text v <+> equals, nest 4 (pTerm t)]
pDecl (DataDecl con tyvars dbody)
    = "data" <+> text con <+> hsep (map text tyvars) $$ nest 4 (pDataBody dbody)

pDataBody :: [(String, Maybe TypeSig)] -> Doc
pDataBody (d : ds) = equals <+> p d $$ vcat (map (\d' -> "|" <+> p d') ds)
  where
    p (s, Nothing) = text s
    p (s, Just ty) = text s <+> parensTy ty
pDataBody _ = "Pretty.pDataBody: Received 0 options"

pTy :: TypeSig -> Doc
pTy (TyCon s) = text s
pTy (TyApp l r) = parensTy l <+> parensTy r
pTy (TyVar v) = text v

parensTy :: TypeSig -> Doc
parensTy t = case t of
    TyApp _ _ -> parens d
    _         -> d
  where
    d = pTy t