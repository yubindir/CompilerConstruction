%{ 
	open Gram3_defs
%}

%token MAIN
%token LRB LCB
%token RRB RCB
%token COMMA SEMICOLON
%token TYPE
%token <string> NAME 
%token <int> INT
%token <bool> TRUE FALSE
%token LET IN
%token ASG DEREF
%token PRINTINT NEWLINE
%token WHILE
%token IF ELSE
%token LEQ GEQ EQUAL NOTEQ
%token PLUS MINUS TIMES DIVIDE
%token AND OR NOT
%token EOF
%start<Gram3_defs.program> program

%%

exp:
(*Sequence*)	| e1 = exp; SEMICOLON; e2 = exp {Seq(e1, e2)}
(*Assign*)		| TYPE; e1 = NAME; ASG; e2 = value {Assign(Id(e1), e2)}
(*Control*)		| e1 = control {e1}
(*Operation*)	| e1 = value {e1}
(*Print INT*)	| PRINTINT; LRB; e1 = exp; RRB {Printint(e1)}
				| NEWLINE; LRB; RRB {Newline}

(*CONDITIONAL STATEMENTS*)
control:
(*If*)		| IF; LRB; e1 = l_exp; RRB; LCB; e2 = exp; RCB; ELSE; LCB; e3 = exp; RCB {If(e1,e2,e3)}
			| IF; LRB; e1 = deref; RRB; LCB; e2 = exp; RCB; ELSE; LCB; e3 = exp; RCB {If(e1,e2,e3)}
(*While*)	| WHILE; LRB; e1 = l_exp; RRB; LCB; e2 = exp; RCB {While(e1,e2)}
			| WHILE; LRB; e1 = deref; RRB; LCB; e2 = exp; RCB {While(e1,e2)}

deref:
	| DEREF; e1 = NAME {Deref(Id(e1))}

value:
(*Arithmetic*)	| e1 = a_exp {e1}
(*Logic*)		| e1 = l_exp {e1}
(*Dereference*)	| e1 = deref {e1}
(*Let*)			| LET; e1 = NAME; ASG; e2 = value; IN; LCB; e3 = exp; RCB {Let(Id(e1),e2,e3)}

(*OPERATIONS*)
a_exp:
	| i=INT {Const(i)}
	| LRB; e1 = a_exp; RRB {e1}
	| e1 = a_exp; op = a_op; e2 = a_exp {Operator(op,e1,e2)} 	| e1 = deref; op = a_op; e2 = a_exp {Operator(op,e1,e2)}
	| e1 = a_exp; op = a_op; e2 = deref {Operator(op,e1,e2)} 	| e1 = deref; op = a_op; e2 = deref {Operator(op,e1,e2)}

l_exp:
	| TRUE {True}
	| FALSE {False}
	| LRB; e1 = l_exp; RRB {e1}
(*Comparison*)
	| e1 = c_exp {e1}

	| NOT; e1 = l_exp {Not(e1)}			| NOT; e1 = deref {Not(e1)}
	| e1 = l_exp; op = l_op; e2 = l_exp {Operator(op,e1,e2)}	| e1 = deref; op = l_op; e2 = l_exp {Operator(op,e1,e2)}
	| e1 = l_exp; op = l_op; e2 = deref {Operator(op,e1,e2)}	| e1 = deref; op = l_op; e2 = deref {Operator(op,e1,e2)}

c_exp:
	| e1 = l_exp; op = c_op; e2 = l_exp {Operator(op,e1,e2)} 	| e1 = deref; op = c_op; e2 = l_exp {Operator(op,e1,e2)} 
	| e1 = l_exp; op = c_op; e2 = deref {Operator(op,e1,e2)} 
	
	| e1 = a_exp; op = c_op; e2 = a_exp {Operator(op,e1,e2)} 	| e1 = deref; op = c_op; e2 = a_exp {Operator(op,e1,e2)} 
	| e1 = a_exp; op = c_op; e2 = deref {Operator(op,e1,e2)} 
	
	| e1 = deref; op = c_op; e2 = deref {Operator(op,e1,e2)}

(*OPERATORS*)
a_op:
	|PLUS {Plus} |MINUS {Minus} |TIMES {Times} |DIVIDE {Divide} 
l_op:
	|AND {And} |OR {Or} 
c_op:
	|LEQ {Leq} |GEQ {Geq} |EQUAL {Equal} |NOTEQ {Noteq}

(*FUNCTION DEFINITIONS*)
fun_definition:
	| fun_name = NAME; LRB; args = separated_list(COMMA, NAME); RRB; LCB; code = exp; RCB {(fun_name, args, code)}
main:
	| MAIN; LRB; args = separated_list(COMMA, NAME); RRB; LCB; code = exp; RCB {("main", args, code)}

(*WHOLE PROGRAM*)
program:
	| fun_list = list(fun_definition); fun_main = main; EOF {fun_main :: fun_list}






