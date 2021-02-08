build: a.out

a.out: gpp_lexer.c
	gcc gpp_lexer.c

gpp_lexer.c: gpp_lexer.l gpp_lexer.h
	lex -o gpp_lexer.c gpp_lexer.l

clean:
	rm a.out
