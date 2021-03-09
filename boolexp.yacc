(* User  declarations *)



%%

%name boolexp

%term ID of string | EOF | TERM | LPAREN | RPAREN | IF | THEN | ELSE 
| IMPLIES | AND | OR | XOR | EQUALS | NOT | CONST of bool

%nonterm START of ABC.program | program of ABC.program | statement of ABC.statement | formula of ABC.formula 

%pos int

%eop EOF

%noshift EOF

%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT

%start START

%verbose

%%
START: program (program)

program: statement (ABC.Statement(statement))
		| program statement (ABC.Program(program,statement))
statement: formula TERM (ABC.Formula(formula,ABC.TERM))
formula:  LPAREN formula RPAREN (ABC.BracExp(ABC.LPAREN,formula,ABC.RPAREN)) 
		| IF formula THEN formula ELSE formula (ABC.IfthenelseExp(ABC.IF,formula1,ABC.THEN,formula2,ABC.ELSE,formula3))
		| formula IMPLIES formula (ABC.ImpliesExp(ABC.IMPLIES,formula1,formula2))
		| formula AND formula (ABC.BinExp(ABC.AND,formula1,formula2))
		| formula OR formula (ABC.BinExp(ABC.OR,formula1,formula2))
		| formula XOR formula (ABC.BinExp(ABC.XOR,formula1,formula2))
		| formula EQUALS formula (ABC.BinExp(ABC.EQUALS,formula1,formula2))
		| NOT formula (ABC.NotExp(ABC.NOT,formula))
		| ID (ABC.ID(ID))
		| CONST (ABC.Const(CONST))
		
