structure ABC =
struct

type id = string

val postorder : string ref = ref ""

datatype typ = FnType of typ * typ
| INT
| BOOL

datatype program = Statement of statement
| Program of program * statement
and statement = Formulaterm of formula * term
| Formula of formula
and formula = VarExp of id
| Const of bool
| ImpliesExp of formula * implies * formula
| IfthenelseExp of If * formula * Then * formula * Else * formula * Fi
| NotExp of not * formula
| BracExp of lparen * formula * rparen
| BinExp of formula * binop * formula
| NumExp of int
| LetExp of Let * id * formula * In * formula * End
| NegExp of neg * formula
| AppExp of formula * formula
| Fn of id * typ * typ * formula
| Fun of id * id * typ * typ * formula
and binop = PLUS | MINUS | TIMES | LESSTHAN | GREATERTHAN | AND | OR | XOR | EQUALS
and neg = NEGATE
and not = NOT
and implies = IMPLIES
and term = TERM
and lparen = LPAREN
and rparen = RPAREN
and If = IF
and Else = ELSE
and Then = THEN
and Fi = FI
and Let = LET
and In = IN
and End = END

datatype value = IntVal of int
				| BoolVal of bool
				| FnVal of id * formula
				
type environment = (id * value) list

val gbev_env:environment ref = ref []

fun envAdd (var:id, v:value, env:environment) =
    (gbev_env:=(var,v)::(!gbev_env);(var,v)::env)

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
	       SOME (x, v)   => v
	    |   NONE => (case List.find(fn (x1, _) => x1 = var) (!gbev_env) of
	       		SOME (x1, v1)   => (case v1 of 
	       								FnVal (a,b) => FnVal(a,b)
	       								|_ => raise Fail ("Environment lookup error for "^ var^" in evaluation ")	
	       							)
	    		|   NONE => raise Fail ("Environment lookup error for "^ var^" in evaluation ")		)					    
end
