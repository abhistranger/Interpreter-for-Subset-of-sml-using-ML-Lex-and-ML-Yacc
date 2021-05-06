(* User  declarations *)



%%

%name boolexp

%term ID of string | EOF | TERM | LPAREN | RPAREN | IF | THEN | ELSE | FI
| IMPLIES | AND | OR | XOR | EQUALS | NOT | CONST of bool | PLUS | MINUS | TIMES | NEGATE 
| LESSTHAN | GREATERTHAN | LET | IN | END | EQ | NUM  of int | FN | INT | BOOL | IMP | COLON | FUN | ARROW

%nonterm START of ABC.program | program of ABC.program | statement of ABC.statement | formula of ABC.formula | Type of ABC.typ

%pos int

%eop EOF

%noshift EOF

%right ARROW

%nonassoc EQ
%nonassoc IMP

%right IF THEN ELSE FI
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%right LESSTHAN GREATERTHAN
%right NEGATE
%left MINUS PLUS
%left TIMES 


%start START

%verbose

%%
START: program (TextIO.output(TextIO.stdOut,"["^substring((!ABC.postorder),0,size((!ABC.postorder))-1)^"]\n");ABC.postorder := "";program)

program: statement (ABC.postorder := (!ABC.postorder)^"program => statement,";ABC.Statement(statement))
		| program statement (ABC.postorder := (!ABC.postorder)^"program => program statement,";ABC.Program(program,statement))
statement: formula TERM (ABC.postorder := (!ABC.postorder)^"TERM ;,statement => formula TERM,";ABC.Formulaterm(formula,ABC.TERM))
		| formula (ABC.postorder := (!ABC.postorder)^"statement => formula,";ABC.Formula(formula))
formula:  LPAREN formula RPAREN (ABC.postorder := (!ABC.postorder)^"LPAREN (,RPAREN ),formula => LPAREN formula RPAREN,";ABC.BracExp(ABC.LPAREN,formula,ABC.RPAREN)) 
		| IF formula THEN formula ELSE formula FI(ABC.postorder := (!ABC.postorder)^"IF if,THEN then,ELSE else,FI fi,"^"formula => IF formula THEN formula ELSE formula FI,";ABC.IfthenelseExp(ABC.IF,formula1,ABC.THEN,formula2,ABC.ELSE,formula3,ABC.FI))
		| formula IMPLIES formula (ABC.postorder := (!ABC.postorder)^"IMPLIES IMPLIES,"^"formula => formula IMPLIES formula,";ABC.ImpliesExp(formula1,ABC.IMPLIES,formula2))
		| formula AND formula (ABC.postorder := (!ABC.postorder)^"AND AND,"^"formula => formula AND formula,";ABC.BinExp(formula1,ABC.AND,formula2))
		| formula OR formula (ABC.postorder := (!ABC.postorder)^"OR OR,"^"formula => formula OR formula,";ABC.BinExp(formula1,ABC.OR,formula2))
		| formula XOR formula (ABC.postorder := (!ABC.postorder)^"XOR XOR,"^"formula => formula XOR formula,";ABC.BinExp(formula1,ABC.XOR,formula2))
		| formula EQUALS formula (ABC.postorder := (!ABC.postorder)^"EQUALS EQUALS,"^"formula => formula EQUALS formula,";ABC.BinExp(formula1,ABC.EQUALS,formula2))
		| NOT formula (ABC.postorder := (!ABC.postorder)^"NOT NOT,"^"formula => NOT formula,";ABC.NotExp(ABC.NOT,formula))
		| CONST (if (CONST=true) then ABC.postorder := (!ABC.postorder)^"CONST TRUE,formula => CONST," else ABC.postorder := (!ABC.postorder)^"CONST FALSE,formula => ID,";ABC.Const(CONST))
		| NUM (ABC.postorder := (!ABC.postorder)^"NUM "^Int.toString(NUM)^",formula => NUM,";ABC.NumExp(NUM))
		| formula PLUS formula (ABC.postorder := (!ABC.postorder)^"PLUS PLUS,"^"formula => formula PLUS formula,";ABC.BinExp(formula1,ABC.PLUS,formula2))
		| formula MINUS formula (ABC.postorder := (!ABC.postorder)^"MINUS MINUS,"^"formula => formula MINUS formula,";ABC.BinExp(formula1,ABC.MINUS,formula2))
		| formula TIMES formula (ABC.postorder := (!ABC.postorder)^"TIMES TIMES,"^"formula => formula TIMES formula,";ABC.BinExp(formula1,ABC.TIMES,formula2))
		| formula LESSTHAN formula (ABC.postorder := (!ABC.postorder)^"LESSTHAN LESSTHAN,"^"formula => formula LESSTHAN formula,";ABC.BinExp(formula1,ABC.LESSTHAN,formula2))
		| formula GREATERTHAN formula (ABC.postorder := (!ABC.postorder)^"GREATERTHAN GREATERTHAN,"^"formula => formula GREATERTHAN formula,";ABC.BinExp(formula1,ABC.GREATERTHAN,formula2))
		| NEGATE formula (ABC.postorder := (!ABC.postorder)^"NEGATE NEGATE,"^"formula => NEGATE formula,";ABC.NegExp(ABC.NEGATE,formula))
		| LET ID EQ formula IN formula END (ABC.postorder := (!ABC.postorder)^"LET let,ID "^ID^",EQ =,IN in,END end,"^"formula => LET ID EQ formula IN formula END,";ABC.LetExp(ABC.LET,ID,formula1,ABC.IN,formula2,ABC.END))
		| LPAREN formula formula RPAREN (ABC.postorder := (!ABC.postorder)^"formula => LPAREN formula formula RPAREN,";ABC.AppExp(formula1,formula2))
		| FN LPAREN ID COLON Type RPAREN COLON Type IMP formula (ABC.postorder := (!ABC.postorder)^"FN fn,"^"formula => FN LPAREN ID COLON Type RPAREN COLON Type IMP formula,";ABC.Fn(ID,Type1,Type2,formula))
		| FUN ID LPAREN ID COLON Type RPAREN COLON Type IMP formula (ABC.postorder := (!ABC.postorder)^"FUN fun,"^"formula => FUN ID LPAREN ID COLON INT ARROW INT RPAREN COLON INT IMP formula,";ABC.Fun(ID1,ID2,Type1,Type2,formula))
		| ID (ABC.postorder := (!ABC.postorder)^"ID "^ID^",formula => ID,";ABC.VarExp(ID))
		
Type:   INT (ABC.postorder := (!ABC.postorder)^"INT int,"^"Type => INT,";ABC.INT)
		| BOOL(ABC.postorder := (!ABC.postorder)^"BOOL bool"^"Type => BOOL,";ABC.BOOL)
		| Type ARROW Type (ABC.postorder := (!ABC.postorder)^"ARROW ->,Type => Type ARROW Type,";ABC.FnType(Type1,Type2))
		| LPAREN Type RPAREN (ABC.postorder := (!ABC.postorder)^"LPAREN (,RPAREN ),Type => LPAREN Type RPAREN,";Type)
		
		
		
