
signature STREAM =
sig
    eqtype s
    eqtype t
    type pos = int * int

    val uncons : s -> (t * s) option
    val move : t -> pos -> pos
    val toString : s -> string
    val toStream : t -> s
end
