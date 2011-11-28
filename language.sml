
(*
 <expr> ::= <var>
          | fn <var> => <expr>
          | <expr> <expr>
          | let <var> = <expr> in <expr>
          | fix <var> -> <expr>
 *)

structure Language :> LANGUAGE =
struct
    type id = string
    datatype expr
      = Var of id
      | Abs of string * expr
      | App of expr * expr
      | Let of id * expr * expr
      | Fix of id * expr

    exception ParseException of string
    exception TypeException of string

    open Parser
    structure S = String

    (* val letter = one_of (S.explode "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") *)
    (* val digit = one_of (S.explode "0123456789") *)
    (* val symbol = one_of ([#"'", #"_"]) *)
    (* val id_p = *)
    (*     letter >>= (fn c => lift (fn s => c :: s) (many (letter ++ digit ++ symbol))) *)
    fun parse s = raise Fail "unimplemented"
    fun typecheck e = raise Fail "unimplemented"
end
