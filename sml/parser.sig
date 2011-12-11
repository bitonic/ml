
signature PARSER =
sig
    type id = string

    datatype expr
      = Var of id
      | Abs of string * expr
      | App of expr * expr
      | Let of id * expr * expr
      | Fix of id * expr
      | IntLit of int

    type file = (id * expr) list

    exception ParseException of string

    val parseExpr : string -> file  (* Raises ParseException *)
    val prettyExpr : expr -> string
end

