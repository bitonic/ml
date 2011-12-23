{-# LANGUAGE OverloadedStrings #-}
module Syntax
       ( Id
       , Decl (..)
       , DataBody
       , TypeSig (..)
       , Literal (..)
       , Pattern (..)
       , Term (..)
         -- * Instances
       , FullTerm
       , DTerm
         -- * Utils
       , tupleCon
       , tupleType
       , tupleTerm
         -- * Pretty printing
       , prettyML
       , prettyDesugar
       ) where

import Text.PrettyPrint

type Id = String

data Decl t = ValDecl Id t
            | DataDecl Id [Id] DataBody
            deriving (Show, Eq)

data TypeSig = TyCon Id
             | TyApp TypeSig TypeSig
             | TyVar Id
             deriving (Show, Eq)

data Literal = IntLit Id
             | RealLit Id
             deriving (Show, Eq)

data Pattern = VarPat Id
             | Pat Id [Pattern]
             | LitPat Literal
             deriving (Show, Eq)

data Term fn lt = Var Id
                | Con Id
                | Abs fn (Term fn lt)
                | App (Term fn lt) (Term fn lt)
                | Let lt (Term fn lt) (Term fn lt)
                | Fix Id (Term fn lt)
                | Literal Literal
                | Case (Term fn lt) [(Pattern, (Term fn lt))]
                deriving (Show, Eq)

type DataBody = [(Id, TypeSig)]

-------------------------------------------------------------------------------

type FullTerm = Term [Pattern] Pattern

type DTerm    = Term Id Id

-------------------------------------------------------------------------------

tupleCon :: Int -> String
tupleCon n = "(" ++ replicate (n - 1) ','  ++ ")"

tupleType :: [TypeSig] -> TypeSig
tupleType ts = foldl TyApp (TyCon (tupleCon (length ts))) ts

tupleTerm :: [Term fn lt] -> Term fn lt
tupleTerm ts = foldl App (Con (tupleCon (length ts))) ts

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
pTerm f l (Fix g t) = "fix" <+> text g <+> "->" <+> pTerm f l t
pTerm _ _ (Literal lit) = pLiteral lit
pTerm f l (Case t cases) = ("case" <+> pTerm f l t <+> "of") $+$
                           nest 4 (pCases (pTerm f l) cases)

parensTerm :: (fn -> Doc) -> (lt -> Doc) -> Term fn lt -> Doc
parensTerm f l t = case t of
    Abs _ _ -> parens d
    Let _ _ _ -> parens d
    Fix _ _ -> parens d
    App _ _ -> parens d
    _ -> d
  where
    d = pTerm f l t

pPattern :: Pattern -> Doc
pPattern (VarPat v) = text v
pPattern (Pat con pts) = parens (text con <+> hsep (map pPattern pts))
pPattern (LitPat lit) = pLiteral lit

pLiteral :: Literal -> Doc
pLiteral (IntLit i) = text (show i)
pLiteral (RealLit r) = text (show r)

pCases :: (a -> Doc) -> [(Pattern, a)] -> Doc
pCases tf (c : cs) = (space <+> p c) $$ vcat (map (\c' -> "|" <+> p c') cs)
  where
    p (pt, t) = pPattern pt <+> "->" <+> tf t
pCases _ _ = "Parser.pCases: Received 0 cases"

pDecl :: (fn -> Doc) -> (lt -> Doc) -> Decl (Term fn lt) -> Doc
pDecl f l (ValDecl v t) = sep ["let" <+> text v <+> equals, nest 4 (pTerm f l t)]
pDecl _ _ (DataDecl con tyvars dbody)
    = "data" <+> text con <+> hsep (map text tyvars) <+> "where" $$
      nest 4 (pDataBody dbody)

pDataBody :: DataBody -> Doc
pDataBody (d : ds) = "  " <> p d $$ vcat (map (\d' -> "|" <+> p d') ds)
  where
    p (s, t) = text s <+> ":" <+> pType t
pDataBody _ = "Parser.pDataBody: Received 0 options"

pType :: TypeSig -> Doc
pType (TyVar v) = text v
pType (TyCon c) = text c
pType (TyApp (TyApp (TyCon "(->)") l) r) = parensType l <+> "->" <+> pType r
pType (TyApp (TyApp (TyCon "(,)") l) r) =
    "(" <> pType l <> "," <+> pType r <> ")"
pType (TyApp (TyApp (TyApp (TyCon "(,,)") l) m) r) =
    "(" <> pType l <> "," <+> pType m <> "," <+> pType r <> ")"
pType (TyApp l r) = parensType l <+> pType r

parensType :: TypeSig -> Doc
parensType t = case t of
    TyApp _ _ -> parens d
    _ -> d
  where
    d = pType t
