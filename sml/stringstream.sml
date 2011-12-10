
structure StringStream :>
          STREAM where type s = string and type t = char =
struct
    type s = string
    type t = char
    type pos = int * int

    structure S = String
    structure L = List

    fun uncons s =
        SOME (L.nth (S.explode (S.substring (s, 0, 1)), 0), S.extract (s, 1, NONE))
        handle Subscrint => NONE

    fun move c (col, row) = if c = #"\t" then (col + 8, row)
                            else if c = #"\n" then (0, row + 1)
                            else (col + 1, row)

    fun toString s = s

    fun toStream c = S.implode [c]
end
