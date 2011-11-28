
functor ParserFun (structure T : TOKEN) :> PARSER where type t = T.t =
struct
    datatype 'r result
      = Success of 'r
      | Fail of string

    type t = T.t

    type stream = T.t list
    type state = T.pos * stream
    type 'r parser = T.pos * stream -> ('r result * state)

    fun parse (p, l) = case p ((0, 0), l) of (r, (_, inp)) => (r, inp)

    fun return x s = (Success x, s)
    fun bind p f s = let fun cont (Success x, s') = f x s'
                           | cont (Fail s, s')    = (Fail s, s')
                       in cont (p s)
                       end

    fun p >>= f = bind p f
    fun l >> r = l >>= (fn _ => r)
    fun lift f p = p >>= (fn x => return (f x))
    fun lift2 f p1 p2 = p1 >>= (fn x => lift (fn y => f (x, y)) p2)
    fun l *> r = l >> r
    fun l <* r = l >>= (fn x => r >> return x)

    fun try p s = case p s
                       of (Fail err, _) => (Fail err, s)
                        | succ          => succ

    fun fail err s = (Fail err, s)
    fun plus l r s = case l s
                        of (Success t, s) => (Success t, s)
                         | _                => r () s

    fun l ++ r = plus l r

    fun any (pos, [])       = (Fail "Parsec.any: no input", (pos, []))
      | any (pos, x :: inp) = (Success x, (T.move (x, pos), inp))
    fun item x = let fun check y = if x = y then return x
                                   else fail "Parsec.item: no match"
                 in any >>= check
                 end
    fun items []        = return []
      | items (x :: xs) = item x >> lift (fn _ => x :: xs) (items xs)
    fun many p = (try p >>= (fn x => lift (fn xs => x :: xs) (many p))) ++ susp (return [])
    fun many1 p = p >>= (fn x => lift (fn xs => x :: xs) (many p))
    fun one_of ps =
        let fun f (x, sum) = sum ++ susp (item x)
        in List.foldr f (fail "Parser.one_of: No items") ps
        end
end
