
structure Utils :> UTILS =
struct
    fun intersperse _ [] = []
      | intersperse x (y :: ys) = y :: x :: intersperse x ys
end
