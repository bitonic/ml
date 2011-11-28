
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

    val exp = String.explode
    val imp = String.implode

    val letter = one_of (exp "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
    val digit = one_of (exp "0123456789")
    val symbol = one_of [#"'", #"_"]
    val space = one_of [#"\t", #" ", #"\n"]

    val id_p = letter >>=
               (fn c => lift (fn s => imp (c :: s)) (many (letter ++ digit ++ symbol)))


    fun parser () =
        let
            val var = lift (fn s => Var s) id_p
            val abs = items (exp "fn") >> space >> items (exp "=>") >> space >> parser ()
            val app = lift2 App (parser ()) (parser ())
        in
            var ++ abs
        end

    fun parse s =
        case Parser.parse (parser (), exp s)
         of (Success e, []) => e
          | (Success _, s)  => raise ParseException ("Leftover input: " ^ imp s)
          | (Fail s, _)     => raise ParseException s
    fun typecheck e = raise General.Fail "unimplemented"
end
