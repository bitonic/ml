
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

    type file = (id * expr) list

    exception ParseException of string

    structure ParsComb = ParsCombFun (structure S = StringStream)
    open ParsComb

    structure S = String
    structure L = List

    val letter =
        oneOf (String.explode "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
    val digit = oneOf [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]
    val symbol = oneOf [#"'", #"_"]
    val space = oneOf [#"\t", #" ", #"\n"]
    val spaces = many space
    fun parens p = (match "(" >> spaces) *> p <* (spaces >> match ")")

    val idP = letter >>=
              (fn c => lift (fn s => S.implode (c :: s))
                            (many (letter ++ digit ++ symbol)))

    val atoi = let fun toDigit c = Char.ord c - Char.ord #"0"
                   fun f [] = 0
                     | f (c :: s) =
                       if Char.isDigit c then
                           toDigit c + 10 * f s
                       else
                           raise General.Fail "atoi: not a number"
               in  f o List.rev
               end

    val intLit = spaces >> lift (IntLit o atoi) (many1 digit)

    val var = spaces >> lift Var idP

    fun abs p = lift2 Abs (match "fn" >> spaces >> idP)
                      (spaces >> match "=>" >> spaces >> p)

    fun app p = lift2 App (p <* many1 space) p

    fun fix p = lift2 Fix (match "fix" >> spaces >> idP)
                      (spaces >> match "=>" >> spaces >> p)

    fun letP p = lift3 Let (match "let" >> spaces >> idP)
                       (spaces >> match "=" >> spaces >> p)
                       (spaces >> match "in" >> spaces >> p)

    fun exprP () =
        let
            fun p () = exprP () ()
        in
            spaces >>
            (try (abs p) ++ try (letP p) ++ try (fix p) ++
             parens (app p) ++ var ++ intLit ++ parens p)
        end

    val fileP = many (lift2 (fn x => x)
                            (match "let" >> spaces >> idP)
                            (spaces *> match "=" *> spaces *> exprP () <* spaces))

    fun parseExpr s =
        case parse (spaces >> fileP) s
         of (Success e, s)        =>
            if S.size s = 0 then e
            else let val err = "Parsing failed, remaining input: " ^ s
                 in (print err; raise ParseException err)
                 end
          | (Fail (s, (c, l)), _) =>
            let val err = "Parsing failed at " ^ Int.toString l ^ "." ^
                          Int.toString c ^ ": " ^ s
            in (print err; raise ParseException err)
            end

    fun prettyExpr (Var v) = v
      | prettyExpr (Abs (v, e)) = "(fn " ^ v ^ " => " ^ prettyExpr e ^ ")"
      | prettyExpr (App (e1, e2)) = prettyExpr e1 ^ " " ^ prettyExpr e2
      | prettyExpr (Let (v, e1, e2)) = "let " ^ v ^ " = " ^ prettyExpr e1 ^ " in " ^
                                       prettyExpr e2
      | prettyExpr (Fix (v, e)) = "fix " ^ v ^ ". " ^ prettyExpr e
      | prettyExpr (IntLit i) = Int.toString i
end
