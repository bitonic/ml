
structure CharToken : TOKEN =
struct
    type t = char
    type pos = int * int

    fun move (c, (col, row)) = if c = #"\t" then (col + 8, row)
                               else if c = #"\n" then (0, row + 1)
                               else (col + 1, row)

    fun to_string c = String.implode [c]
end
