all:
	ml-lex boolexp.lex
	ml-yacc boolexp.yacc
	sml use "loader.sml";
