
signature TYPECHECK =
sig
    exception TypeException of string

    datatype typeExp
      = TyVar of int
      | TyCon of string * typeExp list
      | TyScheme of int * typeExp
      | TyGen of int

    type fileTypes = (Parser.id * typeExp) list

    val baseContext : (string * typeExp) list
    val typecheck : Parser.file -> fileTypes (* Raises TypeException *)
    val prettyType : typeExp -> string
end
