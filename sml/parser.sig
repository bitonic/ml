
signature PARSER =
sig
    type id = string

    datatype expr
      = Var of id
      | Abs of string * expr
      | App of expr * expr
      | Let of id * expr * expr
      | Fix of id * expr
      | Literal of expr literal

    and 'a literal
      = IntLit of string
      | RealLit of string
      | TupleLit of 'a list

    type file = (id * expr) list

    exception ParseException of string

    val parseTokens : Lexer.token list -> file (* Raises ParseException *)
    val prettyExpr : expr -> string
end
