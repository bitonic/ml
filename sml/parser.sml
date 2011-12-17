
(*
 <expr> ::= <var>
          | fn <var> => <expr>
          | <expr> <expr>
          | let <var> = <expr> in <expr>
          | fix <var> => <expr>
 *)

structure Parser :> PARSER =
struct
    type id = string

    datatype expr
      = Var of id
      | Abs of string * expr
      | App of expr * expr
      | Let of id * expr * expr
      | Fix of id * expr
      | Literal of expr literal

    and 'a literal
      = IntLit of string
      | RealLit of string
      | TupleLit of 'a list

    type file = (id * expr) list

    exception ParseException of string

    structure ParsComb = ParsCombFun (structure S = TokenStreamFun (structure T = Lexer))
    open ParsComb
    open Lexer

    structure S = String
    structure L = List
    structure U = Utils

    fun parens p = matchT LPAREN *> p <* matchT RPAREN


    fun tupleLit p = let val sep = matchT COMMA
                     in lift TupleLit
                        (parens ((p <* sep) >>=
                                 (fn e => lift (fn es => e :: es) (sepBy1 p sep))))
                     end

    fun literal p =
        let fun matchIL (INTLIT i) = SOME i
              | matchIL _          = NONE
            fun matchRL (REALLIT f) = SOME f
              | matchRL _           = NONE
        in lift Literal (lift IntLit (matchT' matchIL) ++
                         lift RealLit (matchT' matchRL) ++
                         tupleLit p)
        end

    val idP = let fun matchID (ID i) = SOME i
                    | matchID _      = NONE
              in matchT' matchID
              end

    val var = lift Var idP

    fun abs p = lift2 Abs (matchT LAMBDA >> idP) (matchT ARROW >> p)

    fun fix p = lift2 Fix (matchT FIX >> idP) (matchT ARROW >> p)

    fun letP p = lift3 Let (matchT LET >> idP) (matchT EQUALS >> p) (matchT IN >> p)

    fun exprP () =
        let
            fun p () = exprP () ()
            val ep = try (abs p) ++ try (letP p) ++ try (fix p) ++ try (literal p) ++
                     try var ++ parens p
            fun app [x] = x
              | app [x, y] = App (x, y)
              | app (x :: y :: xs) = App (App (x, y), app xs)
              | app _ = raise General.Fail "Parser.exprP.app: list.length < 1"
        in ep >>= (fn e => lift (fn es => app (e :: es)) (many ep))
        end

    val fileP = many (lift2 (fn x => x) (matchT LET >> idP) (matchT EQUALS >> exprP ()))

    fun parseTokens tks =
        case parse fileP tks
         of (Success e, tks')       =>
            if L.length tks' = 0 then e
            else let val err = "Parsing failed, remaining input."
                 in (print err; raise ParseException err)
                 end
          | (Fail (msg, (c, l)), _) =>
            let val err = "Parsing failed at " ^ Int.toString l ^ "." ^
                          Int.toString c ^ ": " ^ msg
            in (print err; raise ParseException err)
            end

    fun prettyExpr (Var v) = v
      | prettyExpr (Abs (v, e)) = "(\\ " ^ v ^ " -> " ^ prettyExpr e ^ ")"
      | prettyExpr (App (e1, e2)) = prettyExpr e1 ^ " " ^ prettyExpr e2
      | prettyExpr (Let (v, e1, e2)) = "let " ^ v ^ " = " ^ prettyExpr e1 ^ " in " ^
                                       prettyExpr e2
      | prettyExpr (Fix (v, e)) = "fix " ^ v ^ "-> " ^ prettyExpr e
      | prettyExpr (Literal i) = prettyLit i
    and prettyLit (IntLit i) = i
      | prettyLit (RealLit r) = r
      | prettyLit (TupleLit t) = S.concat (["("] @
                                           U.intersperse "," (L.map prettyExpr t) @
                                           [")"])
end
