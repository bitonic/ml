
structure Parser :> PARSER =
struct
    datatype 'r result
      = Success of 'r
      | Fail of string
    type ('t, 'r) parser = 't list -> ('r result * 't list)
    type pos = int

    fun parse (p, l) = p l

    fun return x inp = (Success x, inp)
    fun bind (p, f) inp = let fun cont (Success x, inp') = f x inp'
                                | cont (Fail s, inp')    = (Fail s, inp')
                          in cont (p inp)
                          end
    infix 1 >>=
    fun p >>= f = bind (p, f)
    infix 1 >>
    fun l >> r = l >>= (fn _ => r)
    fun lift f p = p >>= (fn x => return (f x))

    fun fail s inp = (Fail s, inp)
    fun plus (l, r) inp = case l inp
                           of (Success t, inp) => (Success t, inp)
                            | _                => r inp
    infix 1 ++
    fun l ++ r = plus (l, r)

    fun item _ []         = (Fail "Parsec.item: no input", [])
      | item x (y :: inp) = if x = y then (Success x, inp)
                            else (Fail "Parsec.item: no match", y :: inp)
    fun items []        = return []
      | items (x :: xs) = item x >> items xs >>= (fn _ => return (x :: xs))
    fun eof []  = (Success (), [])
      | eof inp = (Fail "Parser.eof: not end of file", inp)
    fun many p = p >>= (fn x => lift (fn xs => x :: xs) (many p)) ++ return []
    fun many1 p = p >>= (fn x => lift (fn xs => x :: xs) (many1 p))
    fun one_of ps =
        List.foldr (fn (x, sum) => sum ++ item x) (fail "Parser.one_of: No items") ps
end
