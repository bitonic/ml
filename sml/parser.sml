(* structure Parser :> PARSER = *)
structure Parser =
struct
    type id = string
    type con = string

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

    datatype typeSig
      = TyCon of string
      | TyApp of typeSig * typeSig
      | TyVar of id

    type dataBody = (con * typeSig option) list

    datatype decl
      = ValDecl of (id * expr)
      | DataDecl of (con * id list * dataBody)

    type file = decl list

    exception ParseException of string

    structure ParsComb = ParsCombFun (structure S = TokenStreamFun (structure T = Lexer))
    open ParsComb
    open Lexer

    structure S = String
    structure L = List
    structure U = Utils

    fun parens p = matchT LPAREN *> p <* matchT RPAREN

    fun parseTup p = let val sep = matchT COMMA
                     in parens ((p <* sep) >>=
                                (fn e => lift (fn es => e :: es) (sepBy1 p sep)))
                     end

    fun tupleLit p = lift TupleLit (parseTup p)

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

    fun app _ [x] = x
      | app con [x, y] = con (x, y)
      | app con (x :: y :: xs) = con (con (x, y), app con xs)
      | app _ _ = raise General.Fail "Parser.app: list.length < 1"

    fun exprP () =
        let
            fun p () = exprP () ()
            val ep = try (abs p) ++ try (letP p) ++ try (fix p) ++ try (literal p) ++
                     try var ++ parens p
        in ep >>= (fn e => lift (fn es => app App (e :: es)) (many ep))
        end

    val con = let fun matchCON (CON c) = SOME c
                    | matchCON _       = NONE
              in matchT' matchCON
              end

    val tyVar = lift TyVar idP

    val tyCon = lift TyCon con

    fun tyTup p =
        let fun tupleCon ts =
                "(" ^ S.concat (L.tabulate (L.length ts, (fn _ => ","))) ^ ")"
            fun tuplize ts =
                L.foldl (fn (r, l) => TyApp (l, r)) (TyCon (tupleCon ts)) ts
        in lift tuplize (parseTup p)
        end

    fun typeSig () =
        let fun recu () = typeSig () ()
            val tp = tyVar ++ tyCon
            val tyApp = tp >>= (fn t => lift (fn ts => app TyApp (t :: ts)) (many tp))
            fun arrowize ts' =
                let val (t :: ts) = L.rev ts'
                in L.foldr (fn (l, r) => TyApp (TyApp (TyCon "(->)", l), r)) t ts
                end
        in (try (tyTup recu) ++ parens recu ++ tyApp) >>=
           (fn t => lift (fn ts => arrowize (t :: ts)) (many (matchT ARROW >> recu)))
        end

    fun dataBody () =
        sepBy1 (lift2 U.id con
                      (lift SOME
                            (tyVar ++ tyCon ++ tyTup (typeSig ()) ++ parens (typeSig ())) ++
                       return NONE))
               (matchT BAR)

    val valDecl =
        lift2 ValDecl (matchT LET >> idP) (matchT EQUALS >> exprP ())

    val dataDecl =
        lift3 DataDecl (matchT DATA >> con) (many idP) (matchT EQUALS >> dataBody ())

    val fileP = many (dataDecl ++ valDecl)

    fun parseTokens tks =
        case parse fileP tks
         of (Success e, tks')       =>
            if L.length tks' = 0 then e
            else let val err = "Parsing failed, remaining input: " ^
                               S.concat (U.intersperse " " (L.map toString tks'))
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
