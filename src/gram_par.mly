%{ 
	open Gram_defs
%}

%token MAIN
%token LBRACKET LBRACE
%token RBRACKET RBRACE
%token COMMA SEMICOLON
%token ID
%token <string> VARNAME 
%token <int> INT
%token <bool> TRUE FALSE
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
%start<Gram_defs.program> program

%%

program:
	| functions = list(function_definition); m = main; EOF {m::functions}
	
main:
	| MAIN; LBRACKET; args = separated_list(COMMA, variable); RBRACKET; LBRACE; c = code; RBRACE {("main", args, c)}

function_definition:
	| name = VARNAME; LBRACKET; args = separated_list(COMMA, variable); RBRACKET; LBRACE; c = code; RBRACE {(name, args, c)}

code:
	| c = code; SEMICOLON; d = code {Seq(c, d)}
	| v = variable; ASG; f = value_exp {Assign(v, f)}
	| v = variable; ASG; READINT; LBRACKET; RBRACKET {Assign(v, Readint)}
	| e = conditional_statement {e}
	| e = io_exp {e}
	| e = function_exp {e}
	| e = value_exp {e}

value_exp:
	| DEREF; v = variable {Deref(v)}
	| e = arithmetic_exp {e}
	| e = comparison_exp {e}
	| e = logic_exp {e}
	| e = environment_exp {e}

(* Would like to pass a function... but i guess a pointer to one makes more sense *)
function_exp:
	|  FUN; name = VARNAME; LBRACKET; args = separated_list(COMMA, value_exp); RBRACKET {Apply(Id(name), args)}

environment_exp:
	| LET; v = variable; ASG; e = value_exp; IN; LBRACE; c = code; RBRACE {Let(v, e, c)}
	| NEW; v = variable; ASG; e = value_exp; IN; LBRACE; c = code; RBRACE {New(v, e, c)}

io_exp:
	| READINT; LBRACKET; RBRACKET {Readint}
	| PRINTINT; LBRACKET; DEREF; v = variable; RBRACKET {Printint(Deref(v))}
	| PRINTINT; LBRACKET; e = arithmetic_exp; RBRACKET {Printint(e)}

conditional_statement:
	| WHILE; LBRACKET; e = comparison_exp; RBRACKET; LBRACE; c = code; RBRACE {While(e, c)}
	| WHILE; LBRACKET; e = logic_exp; RBRACKET; LBRACE; c = code; RBRACE {While(e, c)}
	| IF; LBRACKET; e = comparison_exp; RBRACKET; LBRACE; c = code; RBRACE {If(e, c)}
	| IF; LBRACKET; e = comparison_exp; RBRACKET; LBRACE; c = code; RBRACE; ELSE; LBRACE; d = code; RBRACE {Else(e, c, d)} 
	| IF; LBRACKET; e = logic_exp; RBRACKET; LBRACE; c = code; RBRACE {If(e, c)}
	| IF; LBRACKET; e = logic_exp; RBRACKET; LBRACE; c = code; RBRACE; ELSE; LBRACE; d = code; RBRACE {Else(e, c, d)} 

comparison_exp:
	| LBRACKET; e = comparison_exp; RBRACKET {e}
	| DEREF; v = variable; op = comparison_op; DEREF; w = variable {Operator(op, Deref(v), Deref(w))}
	| DEREF; v = variable; op = comparison_op; f = arithmetic_exp {Operator(op, Deref(v), f)}
	| e = arithmetic_exp; op = comparison_op; DEREF; w = variable {Operator(op, e, Deref(w))}
	| e = arithmetic_exp; op = comparison_op; f = arithmetic_exp {Operator(op, e, f)}

comparison_op:
	| LEQ {Leq}
	| GEQ {Geq}
	| EQUAL {Equal}
	| NOTEQ {Noteq}

arithmetic_exp:
	| LBRACKET; e = arithmetic_exp; RBRACKET {e}
	| i = INT {Const(i)}
	| DEREF; v = variable; op = arithmetic_op; DEREF; w = variable {Operator(op, Deref(v), Deref(w))}
	| DEREF; v = variable; op = arithmetic_op; f = arithmetic_exp {Operator(op, Deref(v), f)}
	| e = arithmetic_exp; op = arithmetic_op; DEREF; w = variable {Operator(op, e, Deref(w))}
	| e = arithmetic_exp; op = arithmetic_op; f = arithmetic_exp {Operator(op, e, f)}

arithmetic_op:
	| PLUS {Plus}
	| MINUS {Minus}
	| TIMES {Times}
	| DIVIDE {Divide}

logic_exp:
	| LBRACKET; e = logic_exp; RBRACKET {e}
	| TRUE {True}
	| FALSE {False}
	| NOT; DEREF; v = variable {Not(Deref(v))}
	| NOT; e = comparison_exp {Not(e)}
	| NOT; e = logic_exp {Not(e)}
	| DEREF; v = variable; op = logic_op; DEREF; w = variable {Operator(op, Deref(v), Deref(w))}
	| DEREF; v = variable; op = logic_op; f = comparison_exp {Operator(op, Deref(v), f)}
	| DEREF; v = variable; op = logic_op; f = logic_exp {Operator(op, Deref(v), f)}
	| e = comparison_exp; op = logic_op; DEREF; w = variable {Operator(op, e, Deref(w))}
	| e = comparison_exp; op = logic_op; f = comparison_exp {Operator(op, e, f)}
	| e = comparison_exp; op = logic_op; f = logic_exp {Operator(op, e, f)}
	| e = logic_exp; op = logic_op; DEREF; w = variable {Operator(op, e, Deref(w))}
	| e = logic_exp; op = logic_op; f = comparison_exp {Operator(op, e, f)}
	| e = logic_exp; op = logic_op; f = logic_exp {Operator(op, e, f)}

logic_op:
	| AND {And}
	| OR {Or}

variable:
	| ID; name = VARNAME {Id(name)}









