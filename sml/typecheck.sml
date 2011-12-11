
structure Typecheck :> TYPECHECK =
struct
    exception TypeException of string

    datatype typeExp
      = TyVar of int
      | TyCon of string * typeExp list
      (* TyGen should appear only in type schemes. *)
      | TyScheme of int * typeExp
      | TyGen of int

    type fileTypes = (Parser.id * typeExp) list

    structure L = List

    val arrCon = "(->)"
    val intCon = "int"

    fun lookup x l = case L.find (fn (y, _) => x = y) l
                      of SOME (_, el) => SOME el
                       | NONE         => NONE

    fun intersect l1 l2 =
        L.filter (fn x => not (L.exists (fn y => x = y) l2)) l1

    open Parser

    (*
     * Applies the substitution to a type.
     * We do not need to worry about quantified type because they are treated
     * differently (TyGen).
     *)
    fun apply sub (TyVar tv)        = (case lookup tv sub
                                        of SOME t => t
                                         | NONE   => TyVar tv)
      | apply sub (TyCon (con, ts)) = TyCon (con, L.map (apply sub) ts)
      | apply sub (TyScheme (n, t)) = TyScheme (n, apply sub t)
      | apply _   t                 = t

    (* Applies the substitution to all the types in a context. *)
    fun applyctx sub = L.map (fn (tv, t) => (tv, apply sub t))

    infix 9 @@
    fun s1 @@ s2 = L.map (fn (tv, t) => (tv, apply s1 t)) s2 @ s1

    (* Gets all the free variables in a type *)
    fun fv (TyVar u)         = [u]
      | fv (TyCon (_, ts))   = L.concat (L.map fv ts)
      | fv (TyScheme (_, t)) = fv t
      | fv t                 = []

    fun fvctx ctx = L.concat (L.map (fn (_, t) => fv t) ctx)

    (*
     * Binds a variable to a type.
     * Checks that that type doesn't contain the variable.
     *)
    fun var_bind tv t =
        if t = TyVar tv then []
        else if L.exists (fn x => x = tv) (fv t) then
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
                L.foldr (fn ((t1, t2), s) => unify (apply s t1) (apply s t2) @@ s) [] tss
        end
      | unify (TyVar tv) t          = var_bind tv t
      | unify t          (TyVar tv) = var_bind tv t
      | unify _          _          = raise TypeException "types do not unify"

    (* Quantifies all the type variables in the provided list. *)
    fun quantify tvs qt =
        let val tvs' = L.filter (fn tv => L.exists (fn x => x = tv) tvs) (fv qt)
            val len  = L.length tvs'
            val s    = L.tabulate (len, (fn i => (L.nth (tvs', i), TyGen i)))
        in TyScheme (len, apply s qt)
        end

    val baseContext =
        [ ("plus", TyCon (arrCon,
                          [TyCon (intCon, []),
                           TyCon (arrCon, [TyCon (intCon, []), TyCon (intCon, [])])]))

        , ("negate", TyCon (arrCon, [TyCon (intCon, []), TyCon (intCon, [])]))
        ]

    (*
     * Type checks a term. Returns the inferred type.
     * If the program is not typeable, raises a TypeException.
     *)
    fun typecheckT ctx t =
        let val counter = ref ~1
            fun fresh () = (counter := !counter + 1; TyVar (!counter))

            fun freshen (TyScheme (n, t)) =
                let val s = L.tabulate (n, (fn i => (i, fresh ())))
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
                in  (s1, apply s1 (TyCon (arrCon, [ty, a])))
                end

              | f ctx (App (e1, e2)) =
                let val ty      = fresh ()
                    val (s1, a) = f ctx e1
                    val (s2, b) = f (applyctx s1 ctx) e2
                    val s3      = unify (apply s2 a) (TyCon (arrCon, [b, ty]))
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
              | f ctx (IntLit i) = ([], TyCon (intCon, []))

        in #2 (f ctx t) handle TypeException s => (print s; raise TypeException s)
        end

    fun typecheck l =
        L.rev (L.foldr (fn ((v, e), ctx) => (v, typecheckT ctx e) :: ctx) [] l)

    fun prettyType (TyVar i) = Int.toString i
      | prettyType (TyCon (con, ts)) =
        let fun pop o' l r = prettyType l ^ " " ^ o' ^ " " ^ prettyType r
            val o' = String.substring (con, 1, (String.size con - 2))
            fun par ts x = if L.length ts > 0 then "(" ^ x ^ ")"
                           else x
        in if String.substring (con, 0, 1) = "(" then
               par ts (pop o' (L.nth (ts, 0)) (L.nth (ts, 1)))
           else
               par ts (L.foldr (fn (t, s) => s ^ " " ^ prettyType t) con ts)
        end
      | prettyType _ = raise General.Fail ""
end
