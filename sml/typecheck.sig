
signature TYPECHECK =
sig
    exception TypeException of string

    datatype constructor
      = Con of string
      | ConOp of string
      | ConTuple of int

    datatype typeExp
      = TyVar of int
      | TyCon of constructor * typeExp list
      (* TyGen should appear only in type schemes. *)
      | TyScheme of int * typeExp
      | TyGen of int

    type fileTypes = (Parser.id * typeExp) list

    val baseContext : (string * typeExp) list
    val typecheck : Parser.file -> fileTypes (* Raises TypeException *)
    val prettyType : typeExp -> string
end
