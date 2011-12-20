{
{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}
module Parser
       ( Decl (..)
       , DataBody
       , TypeSig (..)
       , Literal (..)
       , Term (..)
       , Pattern (..)
       , parseML
         -- * Pretty printing
       , prettyML
       , prettyDecl
       , prettyTerm
       , prettyLiteral
       , prettyDataBody
       ) where

import Data.Foldable (Foldable)
import Data.List (intersperse)
import Data.Traversable (Traversable)
import Text.PrettyPrint

import Lexer (Token (..), Id)

}

%name parseML
%tokentype { Token }
%error { parseError }

%token
  let      { LET }
  in       { IN }
  fix      { FIX }
  '='      { EQUALS }
  '('      { LPAREN }
  ')'      { RPAREN }
  "->"     { ARROW }
  'λ'      { LAMBDA }
  int      { INTLIT $$ }
  real     { REALLIT $$ }
  ','      { COMMA }
  data     { DATA }
  '|'      { BAR }
  var      { VAR $$ }
  con      { CON $$ }
  case     { CASE }
  of       { OF }

%%

Decls : Decl       { [$1] }
      | Decl Decls { $1 : $2 }

Decl : let var '=' Term               { ValDecl $2 $4 }
     | data con TypeVars '=' DataBody { DataDecl $2 (reverse $3) (reverse $5) }

Term : Atom                         { $1 }
     | 'λ' Patterns "->" Term       { Abs (reverse $2) $4 }
     | let Pattern '=' Term in Term { Let $2 $4 $6 }
     | fix var "->" Term            { Fix $2 $4 }
     | case Term of Cases           { Case $2 (reverse $4) }
     | Term Atom                    { App $1 $2 }

Atom : var           { Var $1 }
     | Literal(Term) { Literal $1 }
     | '(' Term ')'  { $2 }

Literal(p) : int      { IntLit $1 }
           | real     { RealLit $1 }
           | Tuple(p) { TupleLit (reverse $1) }

Tuple(p) : '(' TupleBody(p) ')' { $2 }

TupleBody(p) : p ',' p            { [$3, $1] }
             | TupleBody(p) ',' p { $3 : $1 }

Patterns : Patterns Pattern { $2 : $1 }
         | Pattern          { [$1] }

Pattern : var                 { VarPat $1 }
        | con                 { Pat $1 Nothing }
        | '(' con Pattern ')' { Pat $2 (Just $3) }
        | Literal(Pattern)    { LitPat $1 }
        | '(' Pattern ')'     { $2 }
          
Cases : Cases '|' SingleCase { $3 : $1 }
      | SingleCase           { [$1] }

SingleCase : PatternCase "->" Term { ($1, $3) }

PatternCase : con Pattern { Pat $1 (Just $2) }
            | Pattern     { $1 }

TypeVars : var          { [$1] }
         | TypeVars var { $2 : $1 }

DataBody : DataOption              { [$1] }
         | DataBody '|' DataOption { $3 : $1 }

DataOption : con TypeSig { ($1, Just $2) }
           | con         { ($1, Nothing) }

TypeSig : TyAtom         { $1 }
        | TypeSig TyAtom { TyApp $1 $2 }

TyAtom : con             { TyCon $1 }
       | var             { TyVar $1 }
       | Tuple(TypeSig)  { tupleTy (reverse $1) }
       | '(' TypeSig ')' { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Decl = ValDecl Id Term
          | DataDecl Id [Id] DataBody
          deriving (Show, Eq)

data TypeSig = TyCon Id
             | TyApp TypeSig TypeSig
             | TyVar Id
             deriving (Show, Eq)

data Literal a = IntLit Id
               | RealLit Id
               | TupleLit [a]
               deriving (Show, Eq, Functor, Foldable, Traversable)

data Term = Var Id
          | Abs [Pattern] Term
          | App Term Term
          | Let Pattern Term Term
          | Fix Id Term
          | Literal (Literal Term)
          | Case Term [(Pattern, Term)]
          deriving (Show, Eq)

data Pattern = VarPat Id
             | Pat Id (Maybe Pattern)
             | LitPat (Literal Pattern)
             deriving (Show, Eq)

type DataBody = [(Id, Maybe TypeSig)]

tupleTy :: [TypeSig] -> TypeSig
tupleTy ts = foldl TyApp (TyCon op) ts
  where op = "(" ++ replicate (length ts - 1) ','  ++ ")"


------ PRETTY PRINTING --------------------------------------------------------

prettyML :: [Decl] -> String
prettyML = render . vcat . map pDecl

prettyDecl :: Decl -> Doc
prettyDecl = pDecl

prettyTerm :: Term -> Doc
prettyTerm = pTerm

prettyLiteral :: (a -> Doc) -> Literal a -> Doc
prettyLiteral = pLiteral

prettyDataBody :: DataBody -> Doc
prettyDataBody = pDataBody

pTerm :: Term -> Doc
pTerm (Var v) = text v
pTerm (Abs pts t) = "\\" <> hsep (map pPattern pts) <+> "->" <+> pTerm t
pTerm (App t1 t2) = pTerm t1 <+> parensTerm t2
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

pDataBody :: DataBody -> Doc
pDataBody (d : ds) = equals <+> p d $$ vcat (map (\d' -> "|" <+> p d') ds)
  where
    p (s, Nothing) = text s
    p (s, Just ty) = text s <+> parensTy ty
pDataBody _ = "Pretty.pDataBody: Received 0 options"

pTy :: TypeSig -> Doc
pTy (TyCon s) = text s
pTy (TyApp l r) = pTy l <+> parensTy r
pTy (TyVar v) = text v

parensTy :: TypeSig -> Doc
parensTy t = case t of
    TyApp _ _ -> parens d
    _         -> d
  where
    d = pTy t

}
