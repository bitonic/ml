
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
    (* -- Type checking ---------------------------------------------------- *)

    exception TypeException of string

    datatype type_exp
      = TyVar of int
      | TyArr of type_exp * type_exp
      (* TyGen should appear only in type schemes. *)
      | TyScheme of int * type_exp
      | TyGen of int

    fun lookup x l = case List.find (fn (y, _) => x = y) l
                      of SOME (_, el) => SOME el
                       | NONE         => NONE

    fun intersect l1 l2 =
        List.filter (fn x => not (List.exists (fn y => x = y) l2)) l1

    fun apply sub (TyVar tv)        = (case lookup tv sub
                                        of SOME t => t
                                         | NONE   => TyVar tv)
      | apply sub (TyArr (t1, t2))  = TyArr (apply sub t1, apply sub t2)
      | apply sub (TyScheme (n, t)) = TyScheme (n, apply sub t)
      | apply _   t                 = t

    fun applyctx sub = List.map (fn (tv, t) => (tv, apply sub t))

    infix 9 @@
    fun s1 @@ s2 = List.map (fn (tv, t) => (tv, apply s1 t)) s2 @ s1

    fun fv (TyVar u)         = [u]
      | fv (TyArr (t1, t2))  = fv t1 @ fv t2
      | fv (TyScheme (_, t)) = fv t
      | fv t                 = []

    fun fvctx ctx = List.concat (List.map (fn (_, t) => fv t) ctx)

    fun var_bind tv t =
        if t = TyVar tv then []
        else if List.exists (fn x => x = tv) (fv t) then
            raise TypeException "occurs check fails"
        else [(tv, t)]

    fun unify (TyArr (l1, r1)) (TyArr (l2, r2)) =
        let val s1 = unify l1 l2
            val s2 = unify (apply s1 r1) (apply s1 r2)
        in  s2 @@ s1
        end
      | unify (TyVar tv) t          = var_bind tv t
      | unify t          (TyVar tv) = var_bind tv t
      | unify _          _          = raise TypeException "types do not unify"

    fun quantify tvs qt =
        let val tvs' = List.filter (fn tv => List.exists (fn x => x = tv) tvs) (fv qt)
            val len  = List.length tvs'
            val s    = List.tabulate (len, (fn i => (List.nth (tvs', i), TyGen i)))
        in TyScheme (len, apply s qt)
        end

    fun typecheck t =
        let
            val counter = ref ~1
            fun fresh () = (counter := !counter + 1; TyVar (!counter))

            fun freshen (TyScheme (n, t)) =
                let val s = List.tabulate (n, (fn i => (i, fresh ())))
                in apply s t
                end
              | freshen t = t

            fun f ctx (Var v) =
                (case lookup v ctx
                  of NONE   => raise TypeException ("unbound variable " ^ v)
                   | SOME t => ([], freshen t))

              | f ctx (Abs (v, t)) =
                let val ty      = fresh ()
                    val (s1, a) = f ((v, ty) :: ctx) t
                in  (s1, apply s1 (TyArr (ty, a)))
                end

              | f ctx (App (e1, e2)) =
                let val ty      = fresh ()
                    val (s1, a) = f ctx e1
                    val (s2, b) = f (applyctx s1 ctx) e2
                    val s3      = unify (apply s2 a) (TyArr (b, ty))
                in (s3 @@ s2 @@ s1, apply s3 ty)
                end

              | f ctx (Let (v, e1, e2)) =
                let val (s1, a) = f ctx e1
                    val ctx'    = applyctx s1 ctx
                    val a'      = quantify (intersect (fv a) (fvctx ctx')) a
                    val (s2, b) = f ((v, a') :: ctx') e2
                in  (s2 @@ s1, b)
                end

              | f ctx (Fix (v, e)) =
                let val ty      = fresh ()
                    val (s1, a) = f ((v, ty) :: ctx) e
                    val s2      = unify (apply s1 ty) a
                in  (s2 @@ s1, apply s2 a)
                end
        in
            #2 (f [] t) handle TypeException s => (print s; raise TypeException s)
        end
end
