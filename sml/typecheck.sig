
signature TYPECHECK =
sig
    exception TypeException of string

    datatype type_exp
      = TyVar of int
      | TyCon of string * type_exp list
      | TyScheme of int * type_exp
      | TyGen of int

    val baseContext : (string * type_exp) list
    val typecheck : Parser.expr -> type_exp (* Raises TypeException *)
    val prettyType : type_exp -> string
end
