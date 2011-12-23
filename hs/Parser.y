{
{-# LANGUAGE OverloadedStrings #-}
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
       , pDecl
       , pPattern
       ) where

import Data.List (intersperse)
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
  where    { WHERE }
  ':'      { COLON }

%%

Decls : Decl       { [$1] }
      | Decl Decls { $1 : $2 }

Decl : let var '=' Term                 { ValDecl $2 $4 }
     | data con TypeVars where DataBody { DataDecl $2 (reverse $3) (reverse $5) }

Term : Atom                         { $1 }
     | 'λ' Patterns "->" Term       { Abs (reverse $2) $4 }
     | let Pattern '=' Term in Term { Let $2 $4 $6 }
     | fix var "->" Term            { Fix $2 $4 }
     | case Term of Cases           { Case $2 (reverse $4) }
     | Term Atom                    { App $1 $2 }

Atom : var          { Var $1 }
     | con          { Con $1 }
     | Literal      { Literal $1 }
     | Tuple(Term)  { tupleTerm $1 }
     | '(' Term ')' { $2 }

Literal : int      { IntLit $1 }
        | real     { RealLit $1 }

Tuple(p) : '(' TupleBody(p) ')' { $2 }

TupleBody(p) : p ',' p            { [$3, $1] }
             | TupleBody(p) ',' p { $3 : $1 }

PatternAtom : var             { VarPat $1 }
            | Tuple(Pattern)  { Pat (tupleCon (length $1)) $1 }
            | Literal         { LitPat $1 }
            | '(' Pattern ')' { $2 }

Pattern : PatternAtom    { $1 }
        | con PatternCon { Pat $1 (reverse $2) }

PatternCon : PatternCon Pattern { $2 : $1 }
           | Pattern            { [$1] }
           | {- empty -}        { [] }

PatternParens : con         { Pat $1 [] }
              | PatternAtom { $1 }

Patterns : Patterns PatternParens { $2 : $1 }
         | PatternParens          { [$1] }

Cases : Cases '|' SingleCase { $3 : $1 }
      | SingleCase           { [$1] }

SingleCase : Pattern "->" Term { ($1, $3) }

TypeVars : var          { [$1] }
         | TypeVars var { $2 : $1 }

DataBody : DataBody '|' DataOption { $3 : $1 }
         | DataOption              { [$1] }

DataOption : con ':' TypeSig { ($1, $3) }

TypeSig : TyAtom              { $1 }
        | TyAtom "->" TypeSig { TyApp (TyApp (TyCon "(->)") $1) $3 }
        | TypeSig TyAtom      { TyApp $1 $2 }

TyAtom : con             { TyCon $1 }
       | var             { TyVar $1 }
       | Tuple(TypeSig)  { tupleTy (reverse $1) }
       | '(' TypeSig ')' { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

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

tupleCon :: Int -> String
tupleCon n = "(" ++ replicate (n - 1) ','  ++ ")"

tupleTy :: [TypeSig] -> TypeSig
tupleTy ts = foldl TyApp (TyCon (tupleCon (length ts))) ts

tupleTerm :: [Term fn lt] -> Term fn lt
tupleTerm ts = foldl App (Con (tupleCon (length ts))) ts

------ PRETTY PRINTING --------------------------------------------------------

prettyML :: [Decl (Term [Pattern] Pattern)] -> String
prettyML = render . vcat . map (pDecl (hsep . (map pPattern)) pPattern)

pTerm :: (fn -> Doc) -> (lt -> Doc) -> Term fn lt -> Doc
pTerm _ _ (Var v) = text v
pTerm _ _ (Con c) = text c
pTerm f l (Abs pts t) = "\\" <> f pts <+> "->" <+> pTerm f l t
pTerm f l (App t1 t2) = pTerm f l t1 <+> parensTerm f l t2
pTerm f l (Let pt t1 t2) = sep [ "let" <+> l pt <+> equals <+> pTerm f l t1 <+> "in"
                               , pTerm f l t2
                               ]
pTerm f l (Fix g t) = "fix" <+> text g <+> "->" <+> pTerm f l t
pTerm f l (Literal lit) = pLiteral lit
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
}
