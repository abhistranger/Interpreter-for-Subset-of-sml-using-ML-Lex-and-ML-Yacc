structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val x = ref 0
  val col = ref 1
  val error = fn (e, l:int,m:int, _) => (TextIO.output(TextIO.stdOut,"Unknown token:" ^ (Int.toString (l+1)) ^ ":" ^(Int.toString (m)) ^ ":" ^ e ^ "\n"); x := 1)
  val accum = ref ""
  val eof = fn () => if ((!x)=0) then (TextIO.output(TextIO.stdOut,"["^(!accum)^"EOF \"EOF\"]\n");accum := "";col := 1;pos := 0;x := 0; Tokens.EOF(!pos, !pos))
  					else (accum := "";pos := 0;x :=0; Tokens.EOF(!pos, !pos))

  
%%
%header (functor boolexpLexFun(structure Tokens:boolexp_TOKENS));
alpha=[a-zA-Z];
alphaNumUnd=[a-zA-Z0-9_];
digit=[0-9];
ws=[\ \t];
%%
\n 		=> (pos := (!pos) + 1; col := 1; lex());
{ws}+ 	=> (col := (!col)+size(yytext); lex());
"fn"	=> (accum := (!accum)^"FN \"fn\",";col := (!col)+2;Tokens.FN(!pos,!pos));
"->"	=> (accum := (!accum)^"ARROW \"->\",";col := (!col)+2;Tokens.ARROW(!pos,!pos));
"fun"	=> (accum := (!accum)^"FUN \"fun\",";col := (!col)+3;Tokens.FUN(!pos,!pos));
"int"   => (accum := (!accum)^"TYPE \"int\",";col := (!col)+3;Tokens.INT(!pos,!pos));
"bool"  => (accum := (!accum)^"TYPE \"bool\",";col := (!col)+4;Tokens.BOOL(!pos,!pos));
":"		=> (accum := (!accum)^"COLON \":\",";col := (!col)+1;Tokens.COLON(!pos,!pos));	
"=>"	=> (accum := (!accum)^"IMP \"=>\",";col := (!col)+2;Tokens.IMP(!pos,!pos));	
";" 	=> (accum := (!accum)^"TERM \";\",";col := (!col)+1;Tokens.TERM(!pos,!pos));
"TRUE"	=> (accum := (!accum)^"CONST \"TRUE\",";col := (!col)+4;Tokens.CONST(true,!pos,!pos));
"FALSE"	=> (accum := (!accum)^"CONST \"FALSE\",";col := (!col)+5;Tokens.CONST(false,!pos,!pos));
"NOT" 	=> (accum := (!accum)^"NOT \"NOT\",";col := (!col)+3;Tokens.NOT(!pos,!pos));
"AND" 	=> (accum := (!accum)^"AND \"AND\",";col := (!col)+3;Tokens.AND(!pos,!pos));
"OR" 	=> (accum := (!accum)^"OR \"OR\",";col := (!col)+2;Tokens.OR(!pos,!pos));
"XOR" 	=> (accum := (!accum)^"XOR \"XOR\",";col := (!col)+3;Tokens.XOR(!pos,!pos));
"EQUALS" => (accum := (!accum)^"EQUALS \"EQUALS\",";col := (!col)+6;Tokens.EQUALS(!pos,!pos));
"IMPLIES" => (accum := (!accum)^"IMPLIES \"IMPLIES\",";col := (!col)+7;Tokens.IMPLIES(!pos,!pos));
"if" 	=> (accum := (!accum)^"IF \"if\",";col := (!col)+2;Tokens.IF(!pos,!pos));
"then"	=> (accum := (!accum)^"THEN \"then\",";col := (!col)+4;Tokens.THEN(!pos,!pos));
"else"	=> (accum := (!accum)^"ELSE \"else\",";col := (!col)+4;Tokens.ELSE(!pos,!pos));
"fi"	=> (accum := (!accum)^"FI \"fi\",";col := (!col)+4;Tokens.FI(!pos,!pos));
"(" 	=> (accum := (!accum)^"LPAREN \"(\",";col := (!col)+1;Tokens.LPAREN(!pos,!pos));
")" 	=> (accum := (!accum)^"RPAREN \")\",";col := (!col)+1;Tokens.RPAREN(!pos,!pos));
"PLUS"  => (accum := (!accum)^"PLUS \"PLUS\",";col := (!col)+4;Tokens.PLUS(!pos,!pos));
"MINUS"  => (accum := (!accum)^"MINUS \"MINUS\",";col := (!col)+5;Tokens.MINUS(!pos,!pos));
"TIMES"  => (accum := (!accum)^"TIMES \"TIMES\",";col := (!col)+5;Tokens.TIMES(!pos,!pos));
"NEGATE"  => (accum := (!accum)^"NEGATE \"NEGATE\",";col := (!col)+6;Tokens.NEGATE(!pos,!pos));
"LESSTHAN"  => (accum := (!accum)^"LESSTHAN \"LESSTHAN\",";col := (!col)+8;Tokens.LESSTHAN(!pos,!pos));
"GREATERTHAN"  => (accum := (!accum)^"GREATERTHAN \"GREATERTHAN\",";col := (!col)+11;Tokens.GREATERTHAN(!pos,!pos));
"let" => (accum := (!accum)^"LET \"let\",";col := (!col)+3;Tokens.LET(!pos,!pos));
"in" => (accum := (!accum)^"IN \"in\",";col := (!col)+2;Tokens.IN(!pos,!pos));
"end" => (accum := (!accum)^"END \"end\",";col := (!col)+3;Tokens.END(!pos,!pos));
"=" => (accum := (!accum)^"EQ \"=\",";col := (!col)+1;Tokens.EQ(!pos,!pos));
{alpha}{alphaNumUnd}* => (accum := (!accum)^"ID \""^yytext^"\",";col := (!col)+size(yytext);Tokens.ID(yytext,!pos,!pos));
{digit}+ => (accum := (!accum)^"NUM \""^yytext^"\",";col := (!col)+size(yytext);Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !pos));
\r	=> (lex());
.	=> (col := (!col)+1;error (yytext,!pos,!col,!pos);lex());



