
(* Fixity declarations don't follow normal eclipsing rules, which sucks *)
infix 1 >>=
infix 1 >>
infix 2 ++
infix 3 *>
infix 3 <*

signature PARSCOMB =
sig
    datatype 'r result
      = Success of 'r
      | Fail of (string * (int * int))

    eqtype t

    type 'r parser
    type 'r susp = unit -> 'r parser

    val parse : 'r susp * t list -> ('r result * t list)

    val return : 'r -> 'r susp
    val bind : 'a susp -> ('a -> 'b susp) -> 'b susp
    val >>= : 'a susp * ('a -> 'b susp) -> 'b susp
    val >> : 'a susp * 'b susp -> 'b susp
    val lift : ('a -> 'b) -> 'a susp -> 'b susp
    val lift2 : ('a * 'b -> 'c) -> 'a susp -> 'b susp -> 'c susp
    val lift3 : ('a * 'b * 'c -> 'd) -> 'a susp -> 'b susp -> 'c susp -> 'd susp
    val *> : 'a susp * 'b susp -> 'b susp
    val <* : 'a susp * 'b susp -> 'a susp

    val try : 'a susp -> 'a susp

    val fail : string -> 'r susp
    val plus : 'r susp -> 'r susp -> 'r susp
    val ++ : 'r susp * 'r susp -> 'r susp

    val any : t susp
    val eof : unit susp
    val item : t -> t susp
    val items : t list -> (t list) susp
    val many : 'r susp -> ('r list) susp
    val many1 : 'r susp -> ('r list) susp
    val one_of : t list -> t susp
end
