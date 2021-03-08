structure boolexpLrVals = boolexpLrValsFun(structure Token = LrParser.Token)
structure boolexpLex = boolexpLexFun(structure Tokens = boolexpLrVals.Tokens);
structure boolexpParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = boolexpLrVals.ParserData
     	       structure Lex = boolexpLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    boolexpParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  boolexpParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = boolexpLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = boolexpParser.Stream.get lexer
    in
        if boolexpParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer

