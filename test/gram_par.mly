%token LBRACKET RBRACKET
%token LBRACE	RBRACE

%token PLUS MINUS TIMES DIVIDE
%token LEQ GEQ EQUAL NOTEQ
%token AND OR NOT

%token SEMICLN COMMA
%token WHILE
%token IF ELSE
%token ASG DEREF
%token RETURN
%token FUNC
%token INT READINT PRINTINT
%token IDENTIFY LET NEW
%token NAME

%token EOF

%start <int> start
%%

(*
****IDEAS FOR FUTURE****
...Maybe have a keyword for the main function
...separate booleans, strings, integer types 
...separate the boolean and integer operations
...dont use IDENTIFY tokens
*)

start:
	| list(functions) ; EOF {0}

functions:
	| NAME ; LBRACKET ; separated_list(COMMA, exp) ; RBRACKET ; LBRACE ; exp ; RBRACE { } (* Maybe use list(exp) *)

exp:
	| INT 															{ }	(* Constant *)
	| exp 		; SEMICLN 	; exp 									{ } (* Sequence *)
	| exp 		; SEMICLN 											{ } 
 	| WHILE 	; LBRACKET	; exp 		; RBRACKET	; LBRACE	; exp	; RBRACE 	{ } (* While Do *)
	| IF 		; LBRACKET	; exp 		; RBRACKET	; 					
	  LBRACE 	; exp 		; RBRACE								{ } (* If Else*)
	| IF 		; LBRACKET	; exp 		; RBRACKET	; 					
	  LBRACE 	; exp 		; RBRACE
	  ELSE		; LBRACE 	; exp 	; RBRACE						{ }
	| exp 		; ASG 		; exp 									{ } (* Assignment *)
 	| RETURN 	; exp 												{ } (* Return *)
	| DEREF		; exp												{ } (* Dereference *)
	| exp 		; operator 	; exp									{ } (* Operator *)
	| LBRACKET	; exp 		; operator 	; exp		; RBRACKET
	| NOT		; exp												{ }
	| FUNC 		; exp 		; LBRACKET	; separated_list(COMMA, exp) ; RBRACKET 		{ } (* Application *)
	| READINT 	; LBRACKET	; RBRACKET													{ } (* ReadInt *)
	| PRINTINT 	; LBRACKET	; exp 		; RBRACKET 					{ } (* PrintInt *)
	| IDENTIFY 	; NAME 												{ } (* Identifier *)
	| LET 		; IDENTIFY 	; NAME 		; ASG 		; exp 			{ } (* Let ... in *)
	| NEW 		; IDENTIFY 	; NAME 		; ASG 		; exp 			{ } (* New ... in *)

operator:
	| PLUS { }	| MINUS { }	| TIMES { }	| DIVIDE { } (* Arithmetic Operators*)
	| LEQ  { }	| GEQ   { }	| EQUAL { }	| NOTEQ  { } (* Conditional Operators *)
	| AND  { }	| OR    { }	(* Logic Operators*)









