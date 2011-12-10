
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

    exception ParseException of string

    val parse : string -> expr (* Raises ParseException *)
    val prettyExpr : expr -> string
end

