structure Lexer :> LEXER =
struct
    datatype token = LET | IN | FIX | EOF | ID of string | EQUALS
                   | LPAREN | RPAREN | BADCHAR | ARROW | LAMBDA
                   | INTLIT of string | REALLIT of string | COMMA


    structure ParsComb = ParsCombFun (structure S = StringStream)
    open ParsComb

    fun span _ = ((0, 0), (0, 0))

    fun toString LET = "let"
      | toString IN = "in"
      | toString FIX = "fix"
      | toString EOF = "EOF"
      | toString (ID s) = s
      | toString EQUALS = "="
      | toString LPAREN = "("
      | toString RPAREN = ")"
      | toString BADCHAR = "BADCHAR"
      | toString ARROW = "->"
      | toString LAMBDA = "\\"
      | toString (REALLIT f) = f
      | toString (INTLIT i) = i
      | toString COMMA = ","

    structure U = Utils
    structure S = String

    val reserved = [("let", LET), ("in", IN), ("fix", FIX)]
    fun isReserved s = U.lookup s reserved

    val letter =
        oneOf (S.explode "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
    val digit = oneOf [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]
    val symbol = oneOf [#"'", #"_", #"?", #"!"]
    val space = oneOf [#"\t", #" ", #"\n"]
    val spaces = many space
    val spaces1 = many1 space

    val id = lift2 (fn (c, l) => let val s = S.implode (c :: l)
                                 in case isReserved s
                                     of SOME r => r
                                      | _      => ID s
                                 end)
                   letter (many (letter ++ digit ++ symbol))

    val intLit = lift (INTLIT o S.implode) (many1 digit)

    val realLit =
        lift (REALLIT o S.implode)
             (many1 digit >>=
              (fn l => match "." >>
                       lift (fn r => l @ [#"."] @ r) (many1 digit)))

    val token = try realLit ++ intLit ++ id ++
                (match "=" >> return EQUALS) ++
                (match "(" >> return LPAREN) ++
                (match ")" >> return RPAREN) ++
                (match "->" >> return ARROW) ++
                (match "\\" >> return LAMBDA) ++
                (match "," >> return COMMA)

    val lex = spaces *> many (spaces >> token) <* spaces

    fun lexString (s : string) =
        case #1 (parse lex s)
         of Success tks   => tks
          | Fail (err, _) => raise General.Fail err

    fun lexFile f = lexString (TextIO.inputAll (TextIO.openIn f))
end
