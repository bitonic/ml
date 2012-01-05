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
  "=>"     { DOUBLEARROW }
  'λ'      { LAMBDA }
  int      { INTLIT $$ }
  real     { REALLIT $$ }
  ','      { COMMA }
  data     { DATA }
  '|'      { BAR }
  ';'      { SEMICOLON }
  ':'      { COLON }
  var      { VAR $$ }
  con      { CON $$ }
  case     { CASE }
  of       { OF }
  '{'      { LCURLY }
  '}'      { RCURLY }
  class    { CLASS }
  where    { WHERE }
  instance { INSTANCE }

%%


Decls
    : {- empty -}                       { [] }
    | Decl Decls                        { $1 : $2 }

Decl
    : ValDecl                           { $1 }
    | var ':' QualType ';'              { TypeSig $1 $3 }
    | DataDecl                          { $1 }
    | ClassDecl                         { $1 }
    | ClassInst                         { $1 }

ValDecl : var '=' Term ';'              { ValDecl $1 $3 }

DataDecl : data con TypeVars '=' DataBody ';'
                                        { DataDecl $2 (reverse $3) (reverse $5) }

Term
    : Atom                              { $1 }
    | 'λ' Patterns "->" Term            { Abs (reverse $2) $4 }
    | let Pattern '=' Term in Term      { Let $2 $4 $6 }
    | case Term of Cases                { Case $2 (reverse $4) }
    | Term Atom                         { App $1 $2 }

Atom
    : var                               { Var $1 }
    | con                               { Con $1 }
    | Literal                           { Literal $1 }
    | Tuple(Term)                       { tupleTerm $1 }
    | '(' Term ')'                      { $2 }

Literal
    : int                               { IntLit $1 }
    | real                              { RealLit $1 }

Tuple(p)
    : '(' TupleBody(p) ')'              { reverse $2 }

TupleBody(p)
    : p ',' p                           { [$3, $1] }
    | TupleBody(p) ',' p                { $3 : $1 }

PatternAtom
    : var                               { VarPat $1 }
    | Tuple(Pattern)                    { Pat (tupleCon (length $1)) $1 }
    | int                               { IntPat $1 }
    | '(' Pattern ')'                   { $2 }

Pattern
    : PatternAtom                       { $1 }
    | con PatternCon                    { Pat $1 (reverse $2) }

PatternCon
    : PatternCon Pattern                { $2 : $1 }
    | {- empty -}                       { [] }

PatternParens
    : con                               { Pat $1 [] }
    | PatternAtom                       { $1 }

Patterns
    : Patterns PatternParens            { $2 : $1 }
    | PatternParens                     { [$1] }

Cases
    : Cases '|' SingleCase              { $3 : $1 }
    | SingleCase                        { [$1] }

SingleCase : Pattern "->" Term          { ($1, $3) }

-- Reversed
TypeVars
    : var                               { [$1] }
    | TypeVars var                      { $2 : $1 }

-- Reversed
DataBody
    : DataOption                        { [$1] }
    | DataBody '|' DataOption           { $3 : $1 }

DataOption : con Types                  { ($1, reverse $2) }

Types
    : Types TypeAtom                    { $2 : $1 }
    | {- empty -}                       { [] }

QualType
    : Preds "=>" Type                   { $1 :=> $3 }
    | Type                              { [] :=> $1 }

Type
    : TypeApp                           { $1 }
    | TypeApp "->" Type                 { TyApp (TyApp (TyCon (con "(->)")) $1) $3 }

TypeApp
    : TypeAtom                          { $1 }
    | TypeApp TypeAtom                  { TyApp $1 $2 }

TypeAtom
    : con                               { TyCon $1 }
    | var                               { TyVar $1 }
    | Tuple(Type)                       { tupleType $1 }
    | '(' Type ')'                      { $2 }

TypeAtoms
    : TypeAtom                          { [$1] }
    | TypeAtoms TypeAtom                { $2 : $1 }

ClassDecl
    : class con TypeVars where '{' ClassMethods '}'
                                        { ClassDecl [] $2 $3 $6 }
    | class Preds "=>" con TypeVars where '{' ClassMethods '}'
                                        { ClassDecl $2 $4 $5 $8 }

ClassMethods
    : {- empty -}                       { [] }
    | var ':' QualType ';' ClassMethods { ($1, $3) : $5 }

ClassInst
    : instance con TypeAtoms where '{' InstMethods '}'
                                        { ClassInst [] $2 $3 $6 }
    | instance Preds "=>" con TypeAtoms where '{' InstMethods '}'
                                        { ClassInst $2 $4 $5 $8 }

InstMethods
    : {- empty -}                       { [] }
    | var '=' Term ';' InstMethods      { ($1, $3) : $5 }

Pred : con TypeAtoms                    { Pred $1 $2 }

Preds
    : Pred                              { [$1] }
    | '(' Pred ')'                      { [$2] }
    | Tuple(Pred)                       { $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
