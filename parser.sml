
(*
 <expr> ::= <var>
          | fn <var> => <expr>
          | <expr> <expr>
          | let <var> = <expr> in <expr>
          | fix <var> => <expr>
 *)

structure Parser :> PARSER =
(* structure Language = *)
struct
    type id = string
    datatype expr
      = Var of id
      | Abs of string * expr
      | App of expr * expr
      | Let of id * expr * expr
      | Fix of id * expr

    (* --------------------------------------------------------------------- *)
    (* -- Parsing ---------------------------------------------------------- *)

    exception ParseException of string

    structure ParsComb = ParsCombFun (structure T = CharToken)
    open ParsComb

    val exp = String.explode
    val imp = String.implode

    val letter = one_of (exp "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
    val digit = one_of (exp "0123456789")
    val symbol = one_of [#"'", #"_"]
    val space = one_of [#"\t", #" ", #"\n"]
    val spaces = many space
    fun parens p = (item #"(" >> spaces) *> p <* (spaces >> item #")")

    val id_p = letter >>=
               (fn c => lift (fn s => imp (c :: s))
                             (many (letter ++ digit ++ symbol)))

    fun parser () =
        let
            fun p () = parser () ()
            val var = lift Var id_p
            val abs = lift2 Abs (items (exp "fn") >> spaces >> id_p)
                                (spaces >> items (exp "=>") >> spaces >> p)
            val app = lift2 App (p <* many1 space) p
            val let_p = lift3 Let (items (exp "let") >> spaces >> id_p)
                                  (spaces >> item #"=" >> spaces >> p)
                                  (spaces >> items (exp "in") >> spaces >> p)
            val fix = lift2 Fix (items (exp "fix") >> spaces >> id_p)
                                (spaces >> items (exp "=>") >> spaces >> p)
        in
            try abs ++ try let_p ++ try fix ++ parens app ++ var ++ parens p
        end

    fun parse s =
        case ParsComb.parse (parser (), exp s)
         of (Success e, s)        => (e, imp s)
          | (Fail (s, (c, l)), _) =>
            let val err = "Parsing failed at " ^ Int.toString l ^ "." ^
                          Int.toString c ^ ": " ^ s
            in (print err; raise ParseException err)
            end

    (* --------------------------------------------------------------------- *)
    (* -- Type checking ---------------------------------------------------- *)
end
