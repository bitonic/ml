
signature PARSER =
sig
    datatype 'r result
      = Success of 'r
      | Fail of string
    type ('t, 'r) parser
    type pos = int

    val parse : ('t, 'r) parser * 't list -> ('r result * 't list)

    val return : 'r -> ('t, 'r) parser
    val bind : ('t, 'a) parser * ('a -> ('t, 'b) parser) -> ('t, 'b) parser
    val >>= : ('t, 'a) parser * ('a -> ('t, 'b) parser) -> ('t, 'b) parser
    val >> : ('t, 'a) parser * ('t, 'b) parser -> ('t, 'b) parser
    val lift : ('a -> 'b) -> ('t, 'a) parser -> ('t , 'b) parser
    val lift2 : ('a * 'b -> 'c) -> ('t, 'a) parser -> ('t, 'b) parser -> ('t , 'c) parser

    val fail : string -> ('t, 'r) parser
    val plus : ('t, 'r) parser * ('t, 'r) parser -> ('t, 'r) parser
    val ++ : ('t, 'r) parser * ('t, 'r) parser -> ('t, 'r) parser

    val item : ''t -> (''t, ''t) parser
    val items : ''t list -> (''t, ''t list) parser
    val eof : ('t, unit) parser
    val many : (''t, ''r) parser -> (''t, ''r list) parser
    val many1 : (''t, ''r) parser -> (''t, ''r list) parser
    val one_of : ''t list -> (''t, ''t) parser
end
