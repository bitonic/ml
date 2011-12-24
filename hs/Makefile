
lexer: Lexer.x
	alex Lexer.x

parser: Parser.y
	happy -c -g -a Parser.y

i: lexer parser
	ghci Load.hs
