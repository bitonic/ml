
signature TOKEN =
sig
    eqtype t
    type pos = int * int

    val move : (t * pos) -> pos
end
