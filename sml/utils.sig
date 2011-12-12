
signature UTILS =
sig
    (* Lists *)
    val intersperse : 'a -> 'a list -> 'a list
    val lookup : ''a -> (''a * 'b) list -> 'b option
    val sort : ('a -> 'a -> bool) -> 'a list -> 'a list
    val elem : ''a -> ''a list -> bool
    val difference : ''a list -> ''a list -> ''a list
    val nub : ''a list -> ''a list

    (* Functions *)
    val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c
    val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
    val id : 'a -> 'a
    val const : 'a -> 'b -> 'a
    val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
end
