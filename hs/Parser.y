{
module Parser
       ( Decl (..)
       , DataBody
       , TypeSig (..)
       , Literal (..)
       , Term (..)
       , parse
       ) where

import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  let  { LET }
  in   { IN }
  fix  { FIX }
  '='  { EQUALS }
  '('  { LPAREN }
  ')'  { RPAREN }
  "->" { ARROW }
  'λ'  { LAMBDA }
  int  { INTLIT $$ }
  real { REALLIT $$ }
  ','  { COMMA }
  data { DATA }
  '|'  { BAR }
  var  { VAR $$ }
  con  { CON $$ }
  wildcard { WILDCARD }
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
     | Literal(Term)                { Literal $1 }
     | Case                         { $1 }
     | Term Atom                    { App $1 $2 }

Atom : var          { Var $1 }
     | '(' Term ')' { $2 }

Literal(p) : int                  { IntLit $1 }
           | real                 { RealLit $1 }
           | '(' TupleBody(p) ')' { TupleLit (reverse $2) }

TupleBody(p) : p ',' p            { [$3, $1] }
             | TupleBody(p) ',' p { $3 : $1 }

Pattern : wildcard            { WildPat }
        | var                 { VarPat $1 }
        | '(' con Pattern ')' { Pat $2 $3 }
        | Literal(Pattern)    { LitPat $1 }
          
Case : case of Cases { Case (reverse $3) }
  
SingleCase : Pattern "->" Term

Cases : Cases '|' SingleCase { $3 : $1 }
      | SingleCase           { $1 }

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

data Decl = ValDecl String Term
          | DataDecl String [String] DataBody
          deriving (Show, Eq)

data TypeSig = TyCon String
             | TyApp TypeSig TypeSig
             | TyVar String
             deriving (Show, Eq)

data Literal a = IntLit String
               | RealLit String
               | TupleLit [a]
               deriving (Show, Eq)

data Term = Var String
          | Abs [Pattern] Term
          | App Term Term
          | Let Pattern Term Term
          | Fix String Term
          | Literal (Literal Term)
          | Case [(Pattern, Term)]
          deriving (Show, Eq)

data Pattern = VarPat String
             | Pat String Pattern
             | LitPat (Literal Pattern)
             | WildPat
             deriving (Show, Eq)

type DataBody = [(String, Maybe TypeSig)]

}
