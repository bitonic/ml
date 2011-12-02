
structure Typecheck :> TYPECHECK =
struct
    exception TypeException of string

    datatype type_exp
      = TyVar of int
      | TyCon of string * type_exp list
      (* TyGen should appear only in type schemes. *)
      | TyScheme of int * type_exp
      | TyGen of int

    val arr_con = "(->)"
    val int_con = "int"

    fun lookup x l = case List.find (fn (y, _) => x = y) l
                      of SOME (_, el) => SOME el
                       | NONE         => NONE

    fun intersect l1 l2 =
        List.filter (fn x => not (List.exists (fn y => x = y) l2)) l1

    open Parser

    (*
     * Applies the substitution to a type.
     * We do not need to worry about quantified type because they are treated
     * differently (TyGen).
     *)
    fun apply sub (TyVar tv)        = (case lookup tv sub
                                        of SOME t => t
                                         | NONE   => TyVar tv)
      | apply sub (TyCon (con, ts)) = TyCon (con, List.map (apply sub) ts)
      | apply sub (TyScheme (n, t)) = TyScheme (n, apply sub t)
      | apply _   t                 = t

    (* Applies the substitution to all the types in a context. *)
    fun applyctx sub = List.map (fn (tv, t) => (tv, apply sub t))

    infix 9 @@
    fun s1 @@ s2 = List.map (fn (tv, t) => (tv, apply s1 t)) s2 @ s1

    (* Gets all the free variables in a type *)
    fun fv (TyVar u)         = [u]
      | fv (TyCon (_, ts))   = List.concat (List.map fv ts)
      | fv (TyScheme (_, t)) = fv t
      | fv t                 = []

    fun fvctx ctx = List.concat (List.map (fn (_, t) => fv t) ctx)

    (*
     * Binds a variable to a type.
     * Checks that that type doesn't contain the variable.
     *)
    fun var_bind tv t =
        if t = TyVar tv then []
        else if List.exists (fn x => x = tv) (fv t) then
            raise TypeException "occurs check fails"
        else [(tv, t)]

    (*
     * Unifies two types, returning the substitution that will make them
     * equal.
     *)
    fun unify (TyCon (con1, ts1)) (TyCon (con2, ts2)) =
        let fun zip [] [] = []
              | zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys
              | zip _ _ = raise TypeException "different kinds"
            val tss = zip ts1 ts2
        in
            if con1 <> con2 then
                raise TypeException "different constructors"
            else
                List.foldr (fn ((t1, t2), s) => unify (apply s t1) (apply s t2) @@ s) [] tss
        end
      | unify (TyVar tv) t          = var_bind tv t
      | unify t          (TyVar tv) = var_bind tv t
      | unify _          _          = raise TypeException "types do not unify"

    (* Quantifies all the type variables in the provided list. *)
    fun quantify tvs qt =
        let val tvs' = List.filter (fn tv => List.exists (fn x => x = tv) tvs) (fv qt)
            val len  = List.length tvs'
            val s    = List.tabulate (len, (fn i => (List.nth (tvs', i), TyGen i)))
        in TyScheme (len, apply s qt)
        end

    val base_context =
        [ ("plus", TyCon (arr_con, [TyCon (int_con, []), TyCon (arr_con, [TyCon (int_con, []), TyCon (int_con, [])])]))
        , ("negate", TyCon (arr_con, [TyCon (int_con, []), TyCon (int_con, [])]))
        ]
    (*
     * Type checks a term. Returns the inferred type.
     * If the program is not typeable, raises a TypeException.
     *)
    fun typecheck t =
        let val counter = ref ~1
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
                in  (s1, apply s1 (TyCon (arr_con, [ty, a])))
                end

              | f ctx (App (e1, e2)) =
                let val ty      = fresh ()
                    val (s1, a) = f ctx e1
                    val (s2, b) = f (applyctx s1 ctx) e2
                    val s3      = unify (apply s2 a) (TyCon (arr_con, [b, ty]))
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
              | f ctx (IntLit i) = ([], TyCon (int_con, []))

        in #2 (f base_context t) handle TypeException s => (print s; raise TypeException s)
        end

    fun pretty_type (TyVar i) = Int.toString i
      | pretty_type (TyCon (con, ts)) =
        let fun pop o' l r = pretty_type l ^ " " ^ o' ^ " " ^ pretty_type r
            val o' = String.substring (con, 1, (String.size con - 2))
        in if String.substring (con, 0, 1) = "(" then
               "(" ^ pop o' (List.nth (ts, 0)) (List.nth (ts, 1)) ^ ")"
           else
               "(" ^ List.foldr (fn (t, s) => s ^ " " ^ pretty_type t) con ts ^ ")"
        end
      | pretty_type _ = raise General.Fail ""
end
