
functor ParsCombFun (structure S : STREAM) :>
        PARSCOMB where type s = S.s and type t = S.t =
struct
    datatype 'r result
      = Success of 'r
      | Fail of string * S.pos

    type s = S.s
    type t = S.t

    type state = S.pos * s
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

    fun any () (pos, inp) =
        case S.uncons inp
         of SOME (t, inp') => (Success t, (S.move t pos, inp'))
          | NONE           => (Fail ("Parsec.any: no input", pos), (pos, inp))

    fun eof () (pos, inp) =
        case S.uncons inp
         of NONE   => (Success (), (pos, inp))
          | SOME _ => (Fail ("Parsec.eof: input remaining", pos), (pos, inp))

    fun matchT x =
        let val ttostr = S.toString o S.toStream
            fun check y = if x = y then return x
                          else fail ("Parsec.item: received " ^ ttostr y ^
                                     ", expecting " ^ ttostr x ^ ".")
        in any >>= check
        end

    fun match s =
        case S.uncons s
         of NONE         => return s
          | SOME (t, s') => matchT t >> lift (fn _ => s) (match s')

    fun many p = (try p >>= (fn x => lift (fn xs => x :: xs) (many p))) ++ return []

    fun many1 p = p >>= (fn x => lift (fn xs => x :: xs) (many p))

    fun oneOf ps =
        let fun f (x, sum) = sum ++ matchT x
        in List.foldr f (fail "Parser.oneOf: No items") ps
        end
end
