structure ABC =
struct
datatype program = Statement of statement
| Program of program * statement
and statement = Formula of formula * term
and formula = ID of string
| Const of bool
| ImpliesExp of implies * formula * formula
| IfthenelseExp of If * formula * Then * formula * Else * formula
| NotExp of not * formula
| BracExp of lparen * formula * rparen
| BinExp of binop * formula * formula
and binop = AND | OR | XOR | EQUALS
and not = NOT
and implies = IMPLIES
and term = TERM
and lparen = LPAREN
and rparen = RPAREN
and If = IF
and Else = ELSE
and Then = THEN
end 
