
signature TYPECHECK =
sig
    exception TypeException of string

    datatype type_exp
      = TyVar of int
      | TyCon of string * type_exp list
      | TyScheme of int * type_exp
      | TyGen of int

    val typecheck : Parser.expr -> type_exp      (* Raises TypeException *)
    val pretty_type : type_exp -> string
end
