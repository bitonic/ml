
(*
 <expr> ::= <var>
          | fn <var> => <expr>
          | <expr> <expr>
          | let <var> = <expr> in <expr>
          | fix <var> => <expr>
 *)

structure Language :> LANGUAGE =
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

    structure Parser = ParserFun (structure T = CharToken)
    open Parser

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
        case Parser.parse (parser (), exp s)
         of (Success e, s)        => (e, imp s)
          | (Fail (s, (c, l)), _) =>
            let val err = "Parsing failed at " ^ Int.toString l ^ "." ^
                          Int.toString c ^ ": " ^ s
            in (print err; raise ParseException err)
            end

    (* --------------------------------------------------------------------- *)
    (* -- Type checking - Thanks Luca Cardelli & Tim Sheard ---------------- *)

    exception TypeException of string

    datatype type_exp
      = MutVar of (type_exp option) ref
      | GenVar of int
      | OperType of string * type_exp list

    fun prune (t as (MutVar r)) = (case !r
                                    of NONE    => t
                                     | SOME t2 => let val t2' = prune t2
                                                  in  (r := SOME t2'; t2')
                                                  end)
      | prune t                 = t

    fun occurs_in r t =
        case prune t
         of MutVar r2        => r = r2
          | GenVar n         => false
          | OperType (_, ts) => List.foldl (fn (t', b) => b orelse occurs_in r t') false ts

    fun unify t1 t2 =
        let
            val t1' = prune t1
            val t2' = prune t2
            fun unify_args [] [] = ()
              | unify_args (x :: xs) (y :: ys) = (unify x y; unify_args xs ys)
              | unify_args _ _ = raise TypeException "different lengths"
        in
            case (t1', t2')
             of (MutVar r1, MutVar r2) =>
                if t1' = t2' then () else r1 := SOME t2'
              | (MutVar r1, _) =>
                if occurs_in r1 t2' then raise TypeException "occurs in"
                else r1 := SOME t2
              | (_, MutVar _) => unify t2 t1
              | (GenVar n, GenVar m) =>
                if n = m then () else raise TypeException "different genvars"
              | (OperType (n1, ts1), OperType (n2, ts2)) =>
                if n1 = n2 then unify_args ts1 ts2
                else raise TypeException "different constructors"
              | (_, _) => raise TypeException "different types"
        end

    fun typecheck e = raise General.Fail "unimplemented"
end
