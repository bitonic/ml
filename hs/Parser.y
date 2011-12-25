{
module Parser (parseML) where

import Lexer (Token (..))
import Syntax

}

%name parseML
%tokentype { Token }
%error { parseError }

%token
  let      { LET }
  in       { IN }
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
     | case Term of Cases           { Case $2 (reverse $4) }
     | Term Atom                    { App $1 $2 }

Atom : var          { Var $1 }
     | con          { Con $1 }
     | Literal      { Literal $1 }
     | Tuple(Term)  { tupleTerm $1 }
     | '(' Term ')' { $2 }

Literal : int      { IntLit $1 }
        | real     { RealLit $1 }

Tuple(p) : '(' TupleBody(p) ')' { reverse $2 }

TupleBody(p) : p ',' p            { [$3, $1] }
             | TupleBody(p) ',' p { $3 : $1 }

PatternAtom : var             { VarPat $1 }
            | Tuple(Pattern)  { Pat (tupleCon (length $1)) $1 }
            | int             { IntPat $1 }
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

DataBody : DataOption { [$1] }
         | DataBody '|' DataOption { $3 : $1 }

DataOption : con TypeSigs { ($1, reverse $2) }

TypeSigs : TypeSigs TyAtom { $2 : $1 }
         | TyAtom { [$1] }
         | {- empty -} { [] }

TypeSig : TyAtom { $1 }
        | TypeSig TyAtom { TyApp $1 $2 }

TyAtom : con { TyCon $1 }
       | var { TyVar $1 }
       | Tuple(TypeSig) { tupleType $1 }
       | '(' TypeSig ')' { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
