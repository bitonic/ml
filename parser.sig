
(* Fixity declarations don't follow normal eclipsing rules, which sucks *)
infix 1 >>=
infix 1 >>
infix 2 ++
infix 3 *>
infix 3 <*

signature PARSER =
sig
    datatype 'r result
      = Success of 'r
      | Fail of string

    eqtype t

    type 'r parser

    val parse : 'r parser * t list -> ('r result * t list)

    val return : 'r -> 'r parser
    val bind : 'a parser -> ('a -> 'b parser) -> 'b parser
    val >>= : 'a parser * ('a -> 'b parser) -> 'b parser
    val >> : 'a parser * 'b parser -> 'b parser
    val lift : ('a -> 'b) -> 'a parser -> 'b parser
    val lift2 : ('a * 'b -> 'c) -> 'a parser -> 'b parser -> 'c parser
    val *> : 'a parser * 'b parser -> 'b parser
    val <* : 'a parser * 'b parser -> 'a parser

    val try : 'a parser -> 'a parser

    val fail : string -> 'r parser
    val plus : 'r parser -> (unit -> 'r parser) -> 'r parser
    val ++ : 'r parser * (unit -> 'r parser) -> 'r parser

    val any : t parser
    val item : t -> t parser
    val items : t list -> (t list) parser
    val many : 'r parser -> ('r list) parser
    val many1 : 'r parser -> ('r list) parser
    val one_of : t list -> t parser
end
