structure Typecheck :> TYPECHECK =
struct
    exception TypeException of string

    datatype kind = Star
                  | KiArr of kind * kind

    datatype typeExp
      = Var of int * kind
      | Con of string * kind
      | App of typeExp * typeExp
      (* Gen should appear only in type schemes. *)
      | Scheme of kind list * typeExp
      | Gen of int

    type fileTypes = (Parser.id * typeExp) list

    structure P = Parser
    structure L = List
    structure S = String
    structure U = Utils

    fun prettyType (Var (i, _)) = Int.toString i
      | prettyType (Gen i) = "t" ^ Int.toString i
      | prettyType (Scheme (_, t)) = prettyType t
      | prettyType (Con (con, _)) = con
      | prettyType (App (l, r)) =
        "(" ^ prettyType l ^ " " ^ prettyType r ^ ")"

    (* Kind arrows *)
    infixr 3 *->
    fun l *-> r = KiArr (l, r)

    val arrCon = Con ("(->)", Star *-> Star *-> Star)
    val intCon = Con ("Int", Star)
    val realCon = Con ("Real", Star)
    fun tupleCon n = Con ("(" ^ S.concat (L.tabulate (n, (fn _ => ","))) ^ ")",
                          L.foldr (fn (_, k) => Star *-> k) Star (L.tabulate (n, U.id)))

    (* Type arrows *)
    infixr 3 -->
    fun l --> r = App (App (arrCon, l), r)

    val baseContext =
        [ ("plus", intCon --> intCon --> intCon)
        , ("negate", intCon --> intCon)
        ]

    fun kind (Var (_, k)) = k
      | kind (Con (_, k)) = k
      | kind (App (t, _)) =
        (case kind t
          of (KiArr (_, k)) => k
           | _              => raise Fail "Typecheck.kind: malformed type")
      | kind _ = raise Fail "Trying to get the type of a type scheme or scheme variable"

    (*
     * Applies the substitution to a type.
     * We do not need to worry about quantified type because they are treated
     * differently (Gen).
     *)
    fun apply sub (Var tv)        = (case U.lookup tv sub
                                        of SOME t => t
                                         | NONE   => Var tv)
      | apply sub (App (l, r))    = App (apply sub l, apply sub r)
      | apply sub (Scheme (n, t)) = Scheme (n, apply sub t)
      | apply _   t                 = t

    (* Applies the substitution to all the types in a context. *)
    fun applyctx sub = L.map (fn (tv, t) => (tv, apply sub t))

    infix 9 @@
    fun s1 @@ s2 = L.map (fn (tv, t) => (tv, apply s1 t)) s2 @ s1

    (* Gets all the free variables in a type *)
    fun fv t =
        let fun go (Var tv)        = [tv]
              | go (App (l, r))    = fv l @ fv r
              | go (Scheme (_, t)) = fv t
              | go t                 = []
        in U.nub (go t)
        end

    fun fvctx ctx = L.concat (L.map (fn (_, t) => fv t) ctx)

    (*
     * Binds a variable to a type.
     * Checks that that type doesn't contain the variable.
     *)
    fun varBind (tv as (tvi, k)) t =
        if t = Var tv then []
        else if U.elem tv (fv t) then
            raise TypeException ("Occurs check fails when binding \"" ^ Int.toString tvi ^
                                 "\"" ^ " to \"" ^ prettyType t ^ "\"")
        else if k <> kind t then
            raise TypeException "Kinds do not match"
        else [(tv, t)]

    (*
     * Unifies two types, returning the substitution that will make them
     * equal.
     *)
    fun unify (App (l1, r1)) (App (l2, r2)) =
        let val s1 = unify l1 l2
            val s2 = unify (apply s1 r1) (apply s1 r2)
        in  s2 @@ s1
        end
      | unify (Con (con1, k1)) (Con (con2, k2)) =
        if con1 = con2 then
            if k1 = k2 then []
            else raise Fail "Something went wrong, same constructors different kinds"
        else
            raise TypeException "Different constructors"
      | unify (Var tv) t          = varBind tv t
      | unify t          (Var tv) = varBind tv t
      | unify t1         t2         =
        raise TypeException ("Types \"" ^ prettyType t1 ^ "\" and \"" ^
                             prettyType t2 ^ "\" do not unify")

    (* Quantifies all the type variables in the provided list. *)
    fun quantify tvs qt =
        let val len  = L.length tvs
            val s    = L.tabulate (len, (fn i => (L.nth (tvs, i), Gen i)))
        in Scheme ((L.map #2 tvs), apply s qt)
        end

    (*
     * Type checks a term. Returns the inferred type.
     * If the program is not typeable, raises a TypeException.
     *)
    fun typecheckT ctx t =
        let val counter = ref ~1
            fun fresh k = (counter := !counter + 1; Var (!counter, k))

            fun freshen (Scheme (ks, t)) =
                let val s = L.tabulate (L.length ks, (fn i => (i, fresh (L.nth (ks, i)))))
                    fun go (Gen i) = (case U.lookup i s
                                         of NONE   => Gen i
                                          | SOME v => v)
                      | go (App (l, r)) = App (go l, go r)
                      | go (Scheme (i, t)) = Scheme (i, go t)
                      | go t = t
                in go t
                end
              | freshen t = t

            fun go ctx (P.Var v) =
                (case U.lookup v ctx
                  of NONE   => raise TypeException ("unbound variable " ^ v)
                   | SOME t => ([], freshen t))
              | go ctx (P.Abs (v, t)) =
                let val ty      = fresh Star
                    val (s1, a) = go ((v, ty) :: ctx) t
                in  (s1, apply s1 (ty --> a))
                end
              | go ctx (P.App (e1, e2)) =
                let val ty      = fresh Star
                    val (s1, a) = go ctx e1
                    val (s2, b) = go (applyctx s1 ctx) e2
                    val s3      = unify (apply s2 a) (b --> ty)
                in (s3 @@ s2 @@ s1, apply s3 ty)
                end
              | go ctx (P.Let (v, e1, e2)) =
                let val (s1, a) = go ctx e1
                    val ctx'    = applyctx s1 ctx
                    val a'      = quantify (U.difference (fv a) (fvctx ctx')) a
                    val (s2, b) = go ((v, a') :: ctx') e2
                in  (s2 @@ s1, b)
                end
              | go ctx (P.Fix (v, e)) =
                let val ty      = fresh Star
                    val (s1, a) = go ((v, ty) :: ctx) e
                    val s2      = unify (apply s1 ty) a
                in  (s2 @@ s1, apply s2 a)
                end
              | go ctx (P.Literal l) = goLit ctx l

            and goLit ctx (P.IntLit i) = ([], intCon)
              | goLit ctx (P.RealLit r) = ([], realCon)
              | goLit ctx (P.TupleLit es) =
                let val con = tupleCon (L.length es)
                in L.foldr (fn (e, (s1, l)) => let val (s2, a) = go (applyctx s1 ctx) e
                                               in (s2 @@ s1, App (a, l))
                                               end) ([], con) es
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
