{
module Lexer
       ( Token (..)
       , lexML
       ) where

import Syntax

}

%wrapper "basic"

$digit  = 0-9
$alpha  = [a-zA-Z]
$symbol = ['_\?!]
@id     = ($digit | $alpha | $symbol)*

tokens :-
  $white+;
  "--".*;
  let                { const LET }
  in                 { const IN }
  \=                 { const EQUALS }
  \(                 { const LPAREN }
  \)                 { const RPAREN }
  "->"               { const ARROW }
  \\                 { const LAMBDA }
  \,                 { const COMMA }
  "data"             { const DATA }
  \|                 { const BAR }
  "case"             { const CASE }
  "of"               { const OF }
  "where"            { const WHERE }
  \:                 { const COLON }
  $digit+            { INTLIT }
  $digit+ \. $digit+ { REALLIT }
  [a-z]@id           { VAR . var }
  [A-Z]@id           { CON . con }
{

data Token = LET
           | IN
           | VAR Var
           | CON Con
           | EQUALS
           | LPAREN
           | RPAREN
           | ARROW
           | LAMBDA
           | INTLIT String
           | REALLIT String
           | COMMA
           | DATA
           | BAR
           | CASE
           | OF
           | WHERE
           | COLON
           deriving (Show, Eq)

lexML :: String -> [Token]
lexML = alexScanTokens

}
