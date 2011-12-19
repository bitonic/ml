{
module Lexer
       ( Token (..)
       , alexScanTokens
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
  $digit+            { INTLIT }
  $digit+ \. $digit+ { REALLIT }
  \,                 { const COMMA }
  "data"             { const DATA }
  \|                 { const BAR }
  [a-z]@id           { VAR }
  [A-Z]@id           { CON }
  \_@id              { const WILDCARD }
  "case"             { const CASE }
  "of"               { const OF }

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
           | WILDCARD
           | CASE
           | OF
           deriving (Show, Eq)
}
