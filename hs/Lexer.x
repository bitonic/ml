{
module Lexer
       ( Token (..)
       , Id
       , lexML
       ) where
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
  fix                { const FIX}
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
  [a-z]@id           { VAR }
  [A-Z]@id           { CON }

{

type Id = String

data Token = LET
           | IN
           | FIX
           | VAR Id
           | CON Id
           | EQUALS
           | LPAREN
           | RPAREN
           | ARROW
           | LAMBDA
           | INTLIT Id
           | REALLIT Id
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
