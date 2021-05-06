structure Evaluator  =
struct
open ABC

val brokenTypes = Fail "Error in evaluation!"

fun evalExp(e:formula, env:environment):value =
    case e of
		NumExp i => IntVal i
      | VarExp x => envLookup (x, env)
      | Const x	=> BoolVal x
      | ImpliesExp (e1,_,e2) =>
      		(
      			case (evalExp(e1,env),evalExp(e2,env)) of
      				(BoolVal true,BoolVal true) => BoolVal true
      				| (BoolVal false,BoolVal false) => BoolVal true
      				| (BoolVal false,BoolVal true) => BoolVal true
      				| (BoolVal true,BoolVal false) => BoolVal false
      				| (_,_) => raise brokenTypes
      				
      		)
      | IfthenelseExp (_,e1,_,e2,_,e3,_) =>
      		(
      			let
      				val v1 = evalExp(e1,env)
      			in
      				case v1 of 
      					BoolVal true => evalExp(e2,env)
      					|BoolVal false => evalExp(e3,env) 
      					|_ => raise brokenTypes
      			end
      		)
      | NotExp (_,e) => 
      		(
      			let
      				val v1 = evalExp(e,env)
      			in 
      				case v1 of 
      					BoolVal true => BoolVal false
      					|BoolVal false => BoolVal true
      					|_ => raise brokenTypes
      			end
      		)
      | BracExp (_,e,_) => evalExp(e,env)		   				  
      | BinExp (e1,b,e2)  => evalBinExp(b, e1, e2, env)
      | LetExp (_,x,e1,_,e2,_)  =>
			(   let
					val v1 = evalExp (e1, env)
				in
					evalExp(e2, envAdd (x, v1, env))
				end	
			)
	  | NegExp (_,e) =>
	  	(
	  		let val v1=evalExp(e,env)
	  		in 
	  			case v1 of 
	  				IntVal i => IntVal ((~1)*i)
	  				| _ => raise brokenTypes
	  		end
	  	)
	  | AppExp (e1,e2) =>
	  		(
	  			case evalExp(e1,env) of
	  				FnVal(x1,body) => 
	  				(
	  					let val v1= evalExp(e2,env)
	  					in evalExp(body,envAdd(x1,v1,env))
	  					end
	  				)
	  				|_ => raise brokenTypes
	  		)	
	  | Fn (x,_,_,e) => FnVal (x, e)
	  | Fun (fname,x,t1,t2,e) =>  
	  		(envAdd(fname, FnVal (x,e),env);FnVal (x, e))


and evalBinExp(b:binop, e1:formula, e2:formula, env:environment):value =
	case (b, evalExp(e1, env), evalExp(e2, env))  of
		(PLUS, IntVal i1, IntVal i2) => IntVal (i1+i2)
	  |   (MINUS, IntVal i1, IntVal i2) => IntVal (i1-i2)
	  |   (TIMES, IntVal i1, IntVal i2) => IntVal (i1*i2)
	  |   (LESSTHAN, IntVal i1, IntVal i2) => BoolVal (i1<i2)
	  |   (GREATERTHAN, IntVal i1, IntVal i2) => BoolVal (i1>i2)
	  |	  (AND, BoolVal i1, BoolVal i2) => BoolVal (i1 andalso i2)
	  |   (OR, BoolVal i1, BoolVal i2) => BoolVal (i1 orelse i2)
	  |   (XOR, BoolVal i1, BoolVal i2) => BoolVal ((i1 orelse i2) andalso not(i1 andalso i2))
	  |	  (EQUALS,IntVal i1, IntVal i2) => BoolVal (i1=i2)
	  |   (EQUALS,BoolVal i1, BoolVal i2) => BoolVal (i1=i2)
	  |   _  => raise brokenTypes 
  
and intereval(s: statement,env:environment):value =
	case s of
		Formulaterm (f,_) => evalExp(f,env)
		|Formula f => evalExp(f,env)

and evaluate(p: program,env:environment):value =
	case p of
		Statement s => intereval(s,env)
		|Program (p1,s) => (evaluate(p1,env);intereval(s,env))
end
