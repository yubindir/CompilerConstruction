%token MAIN
%token LBRACKET LBRACE
%token RBRACKET RBRACE
%token COMMA SEMICOLON
%token ID VARNAME INT TRUE FALSE
%token LET
%token NEW
%token IN
%token FUN
%token ASG DEREF
%token READINT PRINTINT
%token WHILE
%token IF ELSE
%token LEQ GEQ EQUAL NOTEQ
%token PLUS MINUS TIMES DIVIDE
%token AND OR NOT
%token EOF
%start<int> program

%%

program:
	| list(function_definition); main; EOF {0}
	
main:
	| MAIN; separated_list(COMMA, variable); LBRACE; code; RBRACE {}

function_definition:
	| VARNAME; separated_list(COMMA, variable); LBRACE; code; RBRACE {}

code:
	| code; SEMICOLON; code {}
	| variable; ASG; value_exp {}
	| conditional_statement {}
	| io_exp {}
	| function_exp {}
	| value_exp {}

value_exp:
	| DEREF; variable {}
	| arithmetic_exp {}
	| comparison_exp {}
	| logic_exp {}
	| environment_exp {}

(* Would like to pass a function... but i guess a pointer to one makes more sense *)
function_exp:
	|  FUN; VARNAME; separated_list(COMMA, value_exp) {}

environment_exp:
	| LET; variable; ASG; value_exp; IN; LBRACE; code; RBRACE {}
	| NEW; variable; ASG; value_exp; IN; LBRACE; code; RBRACE {}

io_exp:
	| READINT {}
	| PRINTINT; variable {}
	| PRINTINT; arithmetic_exp {}

conditional_statement:
	| WHILE; LBRACKET; comparison_exp; RBRACKET; LBRACE; code; RBRACE {}
	| WHILE; LBRACKET; logic_exp; RBRACKET; LBRACE; code; RBRACE {}
	| IF; LBRACKET; comparison_exp; RBRACKET; LBRACE; code; RBRACE {}
	| IF; LBRACKET; comparison_exp; RBRACKET; LBRACE; code; RBRACE; ELSE; LBRACE; code; RBRACE {} 
	| IF; LBRACKET; logic_exp; RBRACKET; LBRACE; code; RBRACE {}
	| IF; LBRACKET; logic_exp; RBRACKET; LBRACE; code; RBRACE; ELSE; LBRACE; code; RBRACE {} 

comparison_exp:
	| LBRACKET; comparison_op; RBRACKET {}
	| variable; comparison_op; variable {}
	| variable; comparison_op; arithmetic_exp {}
	| arithmetic_exp; comparison_op; variable {}
	| arithmetic_exp; comparison_op; arithmetic_exp {}

comparison_op:
	| LEQ {}
	| GEQ {}
	| EQUAL {}
	| NOTEQ {}

arithmetic_exp:
	| LBRACKET; arithmetic_exp; RBRACKET {}
	| INT {}
	| variable; arithmetic_op; variable {}
	| variable; arithmetic_op; arithmetic_exp {}
	| arithmetic_exp; arithmetic_op; variable {}
	| arithmetic_exp; arithmetic_op; arithmetic_exp {}

arithmetic_op:
	| PLUS {}
	| MINUS {}
	| TIMES {}
	| DIVIDE {}

logic_exp:
	| LBRACKET; logic_exp; RBRACKET {}
	| TRUE {}
	| FALSE {}
	| NOT; variable {}
	| NOT; comparison_exp {}
	| NOT; logic_exp {}
	| variable; logic_op; variable {}
	| variable; logic_op; comparison_exp {}
	| variable; logic_op; logic_exp {}
	| comparison_exp; logic_op; variable {}
	| comparison_exp; logic_op; comparison_exp {}
	| comparison_exp; logic_op; logic_exp {}
	| logic_exp; logic_op; variable {}
	| logic_exp; logic_op; comparison_exp {}
	| logic_exp; logic_op; logic_exp {}

logic_op:
	| AND {}
	| OR {}

variable:
	| ID; VARNAME {}









