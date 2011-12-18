
signature TYPECHECK =
sig
    exception TypeException of string

    datatype kind = Star
                  | KiArr of kind * kind

    datatype constructor
      = Con of string
      | ConOp of string
      | ConTuple of int

    datatype typeExp
      = TyVar of int * kind
      | TyCon of constructor * kind
      | TyApp of typeExp * typeExp
      (* TyGen should appear only in type schemes. *)
      | TyScheme of kind list * typeExp
      | TyGen of int

    type fileTypes = (Parser.id * typeExp) list

    val baseContext : (string * typeExp) list
    val typecheck : Parser.file -> fileTypes (* Raises TypeException *)
    val prettyType : typeExp -> string
end
