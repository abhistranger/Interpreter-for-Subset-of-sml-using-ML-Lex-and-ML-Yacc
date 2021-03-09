structure boolexpLrVals = boolexpLrValsFun(structure Token = LrParser.Token)
structure boolexpLex = boolexpLexFun(structure Tokens = boolexpLrVals.Tokens);
structure boolexpParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = boolexpLrVals.ParserData
     	       structure Lex = boolexpLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString pos) ^ ":‘‘concerned production rule’’\n")
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
    let 
    	val dummyEOF = boolexpLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
		val (nextToken, lexer) = boolexpParser.Stream.get lexer		
		(*val _ = case result
			of SOME r =>
                      TextIO.output(TextIO.stdOut,"result = " ^ (makestring r) ^ "\n")
            | NONE => ()*)	
    in
    	(*nextToken :: accu*)
    	(*TextIO.output(TextIO.stdOut,(makestring result));*)
        if boolexpParser.sameToken(nextToken, dummyEOF) then result
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer

fun parseFile(infile: string) =
	let 
		val instream = TextIO.openIn infile
		val st = TextIO.input instream
	in 
		parseString st
	end

