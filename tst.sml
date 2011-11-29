
functor TSTFun (structure K :
                          sig
                              eqtype t
                              val leq : t -> t -> bool
                          end) :> MAP where type k = K.t list =
struct
    type k = K.t list

    datatype 'v tst
      = Branch of K.t * ('v tst) * ('v tst) * ('v tst)
      | End of 'v * ('v tst)
      | Null

    type 'v map = 'v tst

    val empty = Null

    fun singleton []       v = End (v, Null)
      | singleton (c :: s) v = Branch (c, Null, singleton s v, Null)

    fun insert []        v  Null                   = End (v, Null)
      | insert []        v  (End (_, t))           = End (v, t)
      | insert []        v  (Branch (c, l, m, r))  = Branch (c, insert [] v l, m, r)
      | insert s         v  Null                   = singleton s v
      | insert s         v1 (End (v2, t))          = End (v2, insert s v1 t)
      | insert (c1 :: s) v  (Branch (c2, l, m, r)) =
        if  c1 = c2 then Branch (c2, l, insert s v m, r)
        else if K.leq c1 c2 then Branch (c2, insert (c1 :: s) v l, m, r)
        else Branch (c2, l, m, insert (c1 :: s) v r)

    fun lookup _         Null                   = NONE
      | lookup []        (End (v, _))           = SOME v
      | lookup []        (Branch (_, l, _, _))  = lookup [] l
      | lookup s         (End (_, t))           = lookup s t
      | lookup (c1 :: s) (Branch (c2, l, m, r)) =
        if c1 = c2 then lookup s m
        else if K.leq c1 c2 then lookup (c1 :: s) l
        else lookup (c1 :: s) r

    fun delete k t = raise Fail "unimplemented"

    fun to_list t = raise Fail "unimplemented"

    fun from_list l = raise Fail "unimplemented"
end
