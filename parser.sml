
functor ParserFun (structure T : TOKEN) :> PARSER where type t = T.t =
struct
    datatype 'r result
      = Success of 'r
      | Fail of string * T.pos

    type t = T.t

    type stream = T.t list
    type state = T.pos * stream
    type 'r parser = state -> ('r result * state)
    type 'r susp = unit -> 'r parser

    fun parse (p, l) = case p () ((0, 0), l) of (r, (_, inp)) => (r, inp)

    fun return x () s = (Success x, s)

    fun bind p f () s = let fun cont (Success x, s') = f x () s'
                              | cont (Fail s, s')    = (Fail s, s')
                        in cont (p () s)
                        end
    fun p >>= f = bind p f

    fun l >> r = l >>= (fn _ => r)

    fun lift f p = p >>= (fn x => return (f x))

    fun lift2 f p1 p2 = p1 >>= (fn x => lift (fn y => f (x, y)) p2)

    fun lift3 f p1 p2 p3 = p1 >>= (fn x => lift2 (fn (y, z) => f (x, y, z)) p2 p3)

    fun l *> r = l >> r
    fun l <* r = l >>= (fn x => r >> return x)

    fun try p () s = case p () s
                      of (Fail err, _) => (Fail err, s)
                       | succ          => succ

    fun fail err () (pos, inp) = (Fail (err, pos), (pos, inp))

    fun plus l r () s = case l () s
                         of (Success t, s) => (Success t, s)
                          | _              => r () s
    fun l ++ r = plus l r

    fun any () (pos, [])       = (Fail ("Parsec.any: no input", pos), (pos, []))
      | any () (pos, x :: inp) = (Success x, (T.move (x, pos), inp))

    fun eof () (pos, []) = (Success (), (pos, []))
      | eof () s         = fail "Parsec.eof: input remaining" () s

    fun item x =
        let fun check y = if x = y then return x
                          else fail ("Parsec.item: received " ^ T.to_string y ^
                                     ", expecting " ^ T.to_string x ^ ".")
        in any >>= check
        end

    fun items []        = return []
      | items (x :: xs) = (item x) >> lift (fn _ => x :: xs) (items xs)

    fun many p = (try p >>= (fn x => lift (fn xs => x :: xs) (many p))) ++ return []

    fun many1 p = p >>= (fn x => lift (fn xs => x :: xs) (many p))

    fun one_of ps =
        let fun f (x, sum) = sum ++ item x
        in List.foldr f (fail "Parser.one_of: No items") ps
        end
end
