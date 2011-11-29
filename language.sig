
signature LANGUAGE =
sig
    type id = string
    datatype expr
      = Abs of string * expr
      | App of expr * expr
      | Fix of id * expr
      | Let of id * expr * expr
      | Var of id

    exception ParseException of string
    exception TypeException of string

    val parse : string -> (expr * string) (* Raises ParseException *)
    val typecheck : expr -> unit          (* Raises TypeException *)
end
