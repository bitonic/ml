{
{-# LANGUAGE DeriveFunctor #-}
module Parser
       ( Decl (..)
       , DataBody
       , TypeSig (..)
       , Literal (..)
       , Term (..)
       , Pattern (..)
       , parseML
       ) where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

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

Decl : var '=' Term                   { ValDecl $1 $3 }
     | data con TypeVars '=' DataBody { DataDecl $2 (reverse $3) (reverse $5) }

Term : Atom                         { $1 }
     | 'λ' Patterns "->" Term       { Abs (reverse $2) $4 }
     | let Pattern '=' Term in Term { Let $2 $4 $6 }
     | fix var "->" Term            { Fix $2 $4 }
     | Literal(Term)                { Literal $1 }
     | case Term of Cases           { Case $2 (reverse $4) }
     | Term Atom                    { App $1 $2 }

Atom : var          { Var $1 }
     | '(' Term ')' { $2 }

Literal(p) : int                  { IntLit $1 }
           | real                 { RealLit $1 }
           | '(' TupleBody(p) ')' { TupleLit (reverse $2) }

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

TypeSig : con             { TyCon $1 }
        | TypeSig TypeSig { TyApp $1 $2 }
        | var             { TyVar $1 }

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

}
