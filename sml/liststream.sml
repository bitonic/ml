
functor TokenStreamFun (structure T : sig
                            eqtype token
                            val span : token -> ((int * int) * (int * int))
                            val toString : token -> string
                        end) :>
        STREAM where type s = T.token list and type t = T.token =
struct
    type s = T.token list
    type t = T.token
    type pos = int * int

    structure L = List
    structure S = String
    structure U = Utils

    fun uncons [] = NONE
      | uncons (x :: xs) = SOME (x, xs)

    fun move t _ = #2 (T.span t)

    val toString = S.concat o (U.intersperse " ") o (map T.toString)

    fun toStream t = [t]
end
