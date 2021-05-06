structure Typing =
struct

open ABC

type type_env = (id * typ) list

val gb_env:type_env ref = ref []

fun type_envLookup(var:id, env:type_env):typ =
    case List.find(fn (x, _) => x = var) env of
	       SOME (x, v)   => v
	    |   NONE => (case List.find(fn (x1, _) => x1 = var) (!gb_env) of
	    			SOME (x1,v1) => v1
	    			| NONE => raise Fail ("variable "^ var^" is without a type")	)
	    
    
fun type_envAdd (var:id, t:typ, env:type_env) =
    (gb_env:=(var,t)::(!gb_env);(var,t)::env)
    

    
fun gettype (e: formula , env:type_env): typ =
	case e of
		VarExp x => type_envLookup(x,env)
	    |Const _ => BOOL
		|ImpliesExp (f1,_,f2) => 
			(
				let
					val t1=gettype(f1,env)
					val t2=gettype(f2,env) 
				in
					if(t1<>BOOL) then
					raise Fail "IMPLIES does not have bool type given"
					else
						if(t2<>BOOL) then
						raise Fail "IMPLIES does not have bool type given"
						else BOOL
				end
			)
		|IfthenelseExp (_,f1,_,f2,_,f3,_) => 
			(
				let
					val t1=gettype(f1,env)
					val t2=gettype(f2,env)
					val t3=gettype(f3,env)
				in
					if(t1<>BOOL) then
						raise Fail "Condition of if command is not of bool type"
					else 
						if(t2<>t3) then 
						raise Fail "Branches have different type in if then else fi"
						else t2
				end
			)
		|NotExp (_,f1) => 
			(
				let
					val t1=gettype(f1,env)
				in
					if(t1<>BOOL) then
						raise Fail "NOT does not have bool type given"
					else BOOL
				end
			)
		|BracExp (_,f,_) => gettype(f,env)
		|BinExp (f1,b,f2) =>
			(case (gettype(f1,env),gettype(f2,env)) of
				(t1,t2) =>
					if (t1=t2) then
						if(t1=INT orelse t1=BOOL) then 
							if (b=EQUALS orelse b=GREATERTHAN orelse b=LESSTHAN) then BOOL
							else t1
						else raise Fail "Binary exp does not have either bool or int type"
					else raise Fail "Binary expression does not have same types"
			)
		|NumExp _ => INT
		|LetExp (_,x,f1,_,f2,_) =>
			(
				gettype(f2,type_envAdd(x,gettype(f1,env),env))
			) 
		|NegExp (_,f) =>
			(case gettype(f,env) of 
				INT => INT
				| _ => raise Fail "NEGATE does not have int type given"
			)
			
		|AppExp (f1,f2) => 
			(case (gettype(f1,env),gettype(f2,env)) of
				(FnType(t1,t2),t3) =>
					if (t1=t3) then t2
					else raise Fail "Function argument does not match"
				| (_,_) =>  raise Fail "Function was expected"
			)
		|Fn (x,t1,t2,e) =>
			( 
				let 
					val etype = gettype(e,type_envAdd(x,t1,env))
				in
					if(etype<>t2) then 
					raise Fail "return type doest not match with Function return type"
					else FnType(t1,t2)
				end
			)
		|Fun (fname,x,t1,t2,e) =>
			(
				let
					val etype = gettype(e,type_envAdd(fname,FnType(t1,t2),type_envAdd(x,t1,env)))
				in
					if(etype<>t2) then 
					raise Fail "return type doest not match with Function return type"
					else FnType(t1,t2)
				end
			)
	

and intertype(s: statement,env:type_env): typ =
	case s of
		Formulaterm (f,_) => gettype(f,env)
		|Formula f => gettype(f,env)

and getType(p: program,env:type_env) =
	case p of
		Statement s => intertype(s,env)
		|Program (p1,s) => (getType(p1,env);intertype(s,env))
		
end
			
