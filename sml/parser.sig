
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
      = IntLit of int
      | RealLit of real
      | TupleLit of 'a list

    type file = (id * expr) list

    exception ParseException of string

    val parseString : string -> file  (* Raises ParseException *)
    val parseFile : string -> file
    val prettyExpr : expr -> string
end
