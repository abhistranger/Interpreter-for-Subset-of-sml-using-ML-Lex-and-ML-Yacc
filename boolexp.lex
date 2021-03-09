structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val x = ref 0
  val error = fn (e, l:int, _) => (TextIO.output(TextIO.stdOut,"Unknown token:" ^ (Int.toString (l+1)) ^ ":" ^ e ^ "\n"); x := 1)
  val accum = ref ""
  val eof = fn () => if ((!x)=0) then (TextIO.output(TextIO.stdOut,"["^(!accum)^"EOF \"EOF\"]\n");accum := "";Tokens.EOF(!pos, !pos))
  					else (accum := "";Tokens.EOF(!pos, !pos))

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%header (functor boolexpLexFun(structure Tokens:boolexp_TOKENS));
alpha=[A-Za-z];
ws = [\ \n\t];
%%x
\n 	=> (pos := (!pos) + 1; lex());
{ws}+ 	=> (lex());
"EOF" 	=> (TextIO.output(TextIO.stdOut,"["^(!accum)^"EOF \"EOF\"]\n");accum := "";Tokens.EOF(!pos,!pos));
";" 	=> (accum := (!accum)^"TERM \";\",";Tokens.TERM(!pos,!pos));
"TRUE"	=> (accum := (!accum)^"CONST \"TRUE\",";Tokens.CONST(true,!pos,!pos));
"FALSE"	=> (accum := (!accum)^"CONST \"FALSE\",";Tokens.CONST(false,!pos,!pos));
"NOT" 	=> (accum := (!accum)^"NOT \"NOT\",";Tokens.NOT(!pos,!pos));
"AND" 	=> (accum := (!accum)^"AND \"AND\",";Tokens.AND(!pos,!pos));
"OR" 	=> (accum := (!accum)^"OR \"OR\",";Tokens.OR(!pos,!pos));
"XOR" 	=> (accum := (!accum)^"XOR \"XOR\",";Tokens.XOR(!pos,!pos));
"EQUALS" => (accum := (!accum)^"EQUALS \"EQUALS\",";Tokens.EQUALS(!pos,!pos));
"IMPLIES" => (accum := (!accum)^"IMPLIES \"IMPLIES\",";Tokens.IMPLIES(!pos,!pos));
"IF" 	=> (accum := (!accum)^"IF \"IF\",";Tokens.IF(!pos,!pos));
"THEN"	=> (accum := (!accum)^"THEN \"THEN\",";Tokens.THEN(!pos,!pos));
"ELSE"	=> (accum := (!accum)^"ELSE \"ELSE\",";Tokens.ELSE(!pos,!pos));
"(" 	=> (accum := (!accum)^"LPAREN \"(\",";Tokens.LPAREN(!pos,!pos));
")" 	=> (accum := (!accum)^"RPAREN \")\",";Tokens.RPAREN(!pos,!pos));
{alpha}+ => (accum := (!accum)^"ID \""^yytext^"\",";Tokens.ID(yytext,!pos,!pos));
. 	=> (error (yytext,!pos,!pos);lex());
             
