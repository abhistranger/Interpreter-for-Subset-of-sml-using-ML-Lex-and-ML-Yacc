structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"Unknown token:" ^ (Int.toString l) ^ ":" ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%header (functor boolexpLexFun(structure Tokens:boolexp_TOKENS));
alpha=[A-Za-z];
ws = [\ \t];
%%x
\n 	=> (pos := (!pos) + 1; lex());
{ws}+ 	=> (lex());
"EOF" 	=> (Tokens.EOF(!pos,!pos));
";" 	=> (Tokens.TERM(!pos,!pos));
"TRUE"	=> (Tokens.CONST(true,!pos,!pos));
"FALSE"	=> (Tokens.CONST(false,!pos,!pos));
"NOT" 	=> (Tokens.NOT(!pos,!pos));
"AND" 	=> (Tokens.AND(!pos,!pos));
"OR" 	=> (Tokens.OR(!pos,!pos));
"XOR" 	=> (Tokens.XOR(!pos,!pos));
"EQUALS" => (Tokens.EQUALS(!pos,!pos));
"IMPLIES" => (Tokens.IMPLIES(!pos,!pos));
"IF" 	=> (Tokens.IF(!pos,!pos));
"THEN"	=> (Tokens.THEN(!pos,!pos));
"ELSE"	=> (Tokens.ELSE(!pos,!pos));
"(" 	=> (Tokens.LPAREN(!pos,!pos));
")" 	=> (Tokens.RPAREN(!pos,!pos));
{alpha}+ => (Tokens.ID(yytext,!pos,!pos));
. 	=> (error (yytext,!pos,!pos);
             lex());
             
