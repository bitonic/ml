
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

    val parse : string -> (expr * string) (* Raises ParseException *)

    exception TypeException of string

    datatype type_exp
      = TyVar of int
      | TyArr of type_exp * type_exp
      | TyScheme of int * type_exp
      | TyGen of int

    val typecheck : expr -> type_exp      (* Raises TypeException *)
end
