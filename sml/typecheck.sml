
structure Typecheck :> TYPECHECK =
struct
    exception TypeException of string

    datatype constructor
      = Con of string
      | ConOp of string
      | ConTuple of int

    datatype typeExp
      = TyVar of int
      | TyCon of constructor * typeExp list
      (* TyGen should appear only in type schemes. *)
      | TyScheme of int * typeExp
      | TyGen of int

    type fileTypes = (Parser.id * typeExp) list

    open Parser
    structure L = List
    structure S = String
    structure U = Utils

    fun prettyType (TyVar i) = Int.toString i
      | prettyType (TyCon (con, ts)) =
        let fun pr ts' = S.concat (U.intersperse " " (L.map prettyType ts'))
        in case con
            of ConOp op'    => prettyType (L.nth (ts, 0)) ^ " " ^ op' ^ " " ^ pr (L.tl ts)
             | Con s        => s ^ " " ^ pr ts
             | ConTuple ts' => S.concat (["("] @
                                         U.intersperse "," (L.map prettyType ts) @
                                         [")"])
        end
      | prettyType (TyGen i) = Int.toString i
      | prettyType (TyScheme (i, t)) = "forall " ^ Int.toString i ^ ". " ^ prettyType t

    val arrCon = ConOp "->"
    val intCon = Con "Int"
    val realCon = Con "Real"

    val baseContext =
        [ ("plus", TyCon (arrCon,
                          [TyCon (intCon, []),
                           TyCon (arrCon, [TyCon (intCon, []), TyCon (intCon, [])])]))

        , ("negate", TyCon (arrCon, [TyCon (intCon, []), TyCon (intCon, [])]))
        ]

    (*
     * Applies the substitution to a type.
     * We do not need to worry about quantified type because they are treated
     * differently (TyGen).
     *)
    fun apply sub (TyVar tv)        = (case U.lookup tv sub
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
    fun fv t =
        let fun go (TyVar u)         = [u]
              | go (TyCon (_, ts))   = U.nub (L.concat (L.map fv ts))
              | go (TyScheme (_, t)) = fv t
              | go t                 = []
        in U.nub (go t)
        end

    fun fvctx ctx = L.concat (L.map (fn (_, t) => fv t) ctx)

    (*
     * Binds a variable to a type.
     * Checks that that type doesn't contain the variable.
     *)
    fun var_bind tv t =
        if t = TyVar tv then []
        else if U.elem tv (fv t) then
            raise TypeException ("Occurs check fails when binding \"" ^ Int.toString tv ^
                                 "\"" ^ " to \"" ^ prettyType t ^ "\"")
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
                L.foldl (fn ((t1, t2), s) => unify (apply s t1) (apply s t2) @@ s) [] tss
        end
      | unify (TyVar tv) t          = var_bind tv t
      | unify t          (TyVar tv) = var_bind tv t
      | unify t1         t2         =
        raise TypeException ("Types \"" ^ prettyType t1 ^ "\" and \"" ^
                             prettyType t2 ^ "\" do not unify")

    (* Quantifies all the type variables in the provided list. *)
    fun quantify tvs qt =
        let val len  = L.length tvs
            val s    = L.tabulate (len, (fn i => (L.nth (tvs, i), TyGen i)))
        in TyScheme (len, apply s qt)
        end

    fun quantifyLet ctx t = quantify (U.difference (U.nub (fv t)) (fvctx ctx)) t

    (*
     * Type checks a term. Returns the inferred type.
     * If the program is not typeable, raises a TypeException.
     *)
    fun typecheckT ctx t =
        let val counter = ref ~1
            fun fresh () = (counter := !counter + 1; TyVar (!counter))

            fun freshen (TyScheme (n, t)) =
                let val s = L.tabulate (n, (fn i => (i, fresh ())))
                    fun go (TyGen i) = (case U.lookup i s
                                         of NONE   => TyGen i
                                          | SOME v => v)
                      | go (TyVar v) = TyVar v
                      | go (TyCon (con, ts)) = TyCon (con, L.map go ts)
                      | go (TyScheme (i, t)) = TyScheme (i, go t)
                in go t
                end
              | freshen t = t

            fun go ctx (Var v) =
                (case U.lookup v ctx
                  of NONE   => raise TypeException ("unbound variable " ^ v)
                   | SOME t => ([], freshen t))

              | go ctx (Abs (v, t)) =
                let val ty      = fresh ()
                    val (s1, a) = go ((v, ty) :: ctx) t
                in  (s1, apply s1 (TyCon (arrCon, [ty, a])))
                end

              | go ctx (App (e1, e2)) =
                let val ty      = fresh ()
                    val (s1, a) = go ctx e1
                    val (s2, b) = go (applyctx s1 ctx) e2
                    val s3      = unify (apply s2 a) (TyCon (arrCon, [b, ty]))
                in (s3 @@ s2 @@ s1, apply s3 ty)
                end

              | go ctx (Let (v, e1, e2)) =
                let val (s1, a) = go ctx e1
                    val ctx'    = applyctx s1 ctx
                    val a'      = quantify (U.difference (fv a) (fvctx ctx')) a
                    val (s2, b) = go ((v, a') :: ctx') e2
                in  (s2 @@ s1, b)
                end

              | go ctx (Fix (v, e)) =
                let val ty      = fresh ()
                    val (s1, a) = go ((v, ty) :: ctx) e
                    val s2      = unify (apply s1 ty) a
                in  (s2 @@ s1, apply s2 a)
                end
              | go ctx (Literal l) = goLit ctx l

            and goLit ctx (IntLit i) = ([], TyCon (intCon, []))
              | goLit ctx (RealLit r) = ([], TyCon (realCon, []))
              | goLit ctx (TupleLit es) =
                let val ts = L.foldr
                             (fn (e, (s1, l)) => let val (s2, a) = go (applyctx s1 ctx) e
                                                 in (s2 @@ s1, a :: l)
                                                 end) ([], []) es
                in  (#1 ts, TyCon (ConTuple (L.length es), #2 ts))
                end
        in #2 (go ctx t) handle TypeException s => (print s; raise TypeException s)
        end

    fun typecheck l =
        let fun go ctx [] = ctx
              | go ctx ((v, e) :: decls) =
                let val t = typecheckT ctx e
                in go ((v, quantify (fv t) t) :: ctx) decls
                end
        in L.rev (go baseContext l)
        end
end
