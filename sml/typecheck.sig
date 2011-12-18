signature TYPECHECK =
sig
    exception TypeException of string

    datatype kind = Star
                  | KiArr of kind * kind

    datatype typeExp
      = Var of int * kind
      | Con of string * kind
      | App of typeExp * typeExp
      (* Gen should appear only in type schemes. *)
      | Scheme of kind list * typeExp
      | Gen of int

    type fileTypes = (Parser.id * typeExp) list

    val baseContext : (string * typeExp) list
    val typecheck : Parser.file -> fileTypes (* Raises TypeException *)
    val prettyType : typeExp -> string
end
