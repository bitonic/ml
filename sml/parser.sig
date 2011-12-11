
signature PARSER =
sig
    type id = string

    datatype expr
      = Var of id
      | Abs of string * expr
      | App of expr * expr
      | Let of id * expr * expr
      | Fix of id * expr
      | Literal of literal

    and literal
      = IntLit of int
      | RealLit of real
      | TupleLit of expr list

    type file = (id * expr) list

    exception ParseException of string

    val parseFile : string -> file  (* Raises ParseException *)
    val prettyExpr : expr -> string
end

