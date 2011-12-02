
(*
 <expr> ::= <var>
          | fn <var> => <expr>
          | <expr> <expr>
          | let <var> = <expr> in <expr>
          | fix <var> => <expr>
 *)

structure Parser :> PARSER =
(* structure Parser = *)
struct
    type id = string
    datatype expr
      = Var of id
      | Abs of string * expr
      | App of expr * expr
      | Let of id * expr * expr
      | Fix of id * expr
      | IntLit of int

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

    val atoi = let fun to_digit c = Char.ord c - Char.ord #"0"
                   fun f [] = 0
                     | f (c :: s) =
                       if Char.isDigit c then
                           to_digit c + 10 * f s
                       else
                           raise General.Fail "atoi: not a number"
               in  f o List.rev
               end

    fun parser () =
        let
            fun p () = parser () ()
            val int_lit = spaces >> lift (IntLit o atoi) (many1 digit)
            val var = spaces >> lift Var id_p
            val abs = lift2 Abs (items (exp "fn") >> spaces >> id_p)
                                (spaces >> items (exp "=>") >> spaces >> p)
            val app = lift2 App (p <* many1 space) p
            val let_p = lift3 Let (items (exp "let") >> spaces >> id_p)
                                  (spaces >> item #"=" >> spaces >> p)
                                  (spaces >> items (exp "in") >> spaces >> p)
            val fix = lift2 Fix (items (exp "fix") >> spaces >> id_p)
                                (spaces >> items (exp "=>") >> spaces >> p)
        in
            spaces >>
            (try abs ++ try let_p ++ try fix ++ parens app ++ var ++ int_lit ++ parens p)
        end

    fun parse s =
        case ParsComb.parse (parser (), exp s)
         of (Success e, s)        =>
            if List.null s then e
            else let val err = "Parsing failed, remaining input: " ^ imp s
                 in (print err; raise ParseException err)
                 end
          | (Fail (s, (c, l)), _) =>
            let val err = "Parsing failed at " ^ Int.toString l ^ "." ^
                          Int.toString c ^ ": " ^ s
            in (print err; raise ParseException err)
            end

    fun pretty_expr (Var v) = v
      | pretty_expr (Abs (v, e)) = "(fn " ^ v ^ " => " ^ pretty_expr e ^ ")"
      | pretty_expr (App (e1, e2)) = pretty_expr e1 ^ " " ^ pretty_expr e2
      | pretty_expr (Let (v, e1, e2)) = "let " ^ v ^ " = " ^ pretty_expr e1 ^ " in " ^
                                        pretty_expr e2
      | pretty_expr (Fix (v, e)) = "fix " ^ v ^ ". " ^ pretty_expr e
      | pretty_expr (IntLit i) = Int.toString i
end
