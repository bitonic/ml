
signature LEXER =
sig
    datatype token = LET | IN | FIX | EOF | ID of string | EQUALS
                   | LPAREN | RPAREN | BADCHAR | ARROW | LAMBDA
                   | INTLIT of string | REALLIT of string | COMMA

    val span : token -> ((int * int) * (int * int))
    val toString : token -> string

    val lexFile : string -> token list
    val lexString : string -> token list
end
