{
module Lexer
       ( Token (..)
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
  $digit+            { INTLIT }
  $digit+ \. $digit+ { REALLIT }
  [a-z]@id           { VAR }
  [A-Z]@id           { CON }
  \_@id              { WILDCARD }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = LET
           | IN
           | FIX
           | VAR String
           | CON String
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
           | WILDCARD String
           | CASE
           | OF
           deriving (Show, Eq)

lexML :: String -> [Token]
lexML = alexScanTokens

}
