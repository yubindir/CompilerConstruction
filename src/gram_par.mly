%{

let store = Hashtbl.create 100;;

let add2store x y = Hashtbl.add store x y;;
let findValue x = Hashtbl.find store x;;

%}

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
%token <int> INT
%token READINT PRINTINT
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
	| NAME ; LBRACKET ; separated_list(COMMA, exp) ; RBRACKET ; LBRACE ; e = exp ; RBRACE 		{e} (* Maybe use list(exp) *)

exp:
	| i = INT 											{i} (* Constant *)
	| exp 		; SEMICLN 	; exp 								{0} (* Sequence *)
	| exp 		; SEMICLN 									{0}
 	| WHILE 	; LBRACKET	; exp 		; RBRACKET	; LBRACE; exp	; RBRACE 	{0} (* While Do *)
	| IF 		; LBRACKET	; exp 		; RBRACKET	; 					
	  LBRACE 	; exp 		; RBRACE							{0} (* If Else*)
	| IF 		; LBRACKET	; exp 		; RBRACKET	; 					
	  LBRACE 	; exp 		; RBRACE
	  ELSE		; LBRACE 	; exp 		; RBRACE					{0}
	| IDENTIFY	; NAME 	; ASG 	; exp {0} (* Assignment *)
 	| RETURN 	; exp 										{0} (* Return *)
	| DEREF		; IDENTIFY	; NAME {0} (* Dereference *)
	| arithmetic_exp									{0} (* Operators *)
	| LBRACKET	; arithmetic_exp	; RBRACKET
	| logical_exp
	| LBRACKET	; logical_exp		; RBRACKET
	| NOT		; exp										{0}
	| FUNC 		; exp 		; LBRACKET	; separated_list(COMMA, exp) ; RBRACKET 	{0} (* Application *)
	| READINT 	; LBRACKET	; RBRACKET							{0} (* ReadInt *)
	| PRINTINT 	; LBRACKET	; exp 		; RBRACKET 					{0} (* PrintInt *)
	| LET 		; IDENTIFY 	; NAME 		; ASG 		; exp 				{0} (* Let ... in *)
	| NEW 		; IDENTIFY 	; NAME 		; ASG 		; exp 				{0} (* New ... in *)

arithmetic_exp:
	| e = INT 	; PLUS 		; f = INT {print_int(e + f); print_string("\n"); e + f}	
	| e = INT 	; MINUS 	; f = INT {print_int(e - f); print_string("\n"); e - f}	
	| e = INT 	; TIMES		; f = INT {print_int(e * f); print_string("\n"); e * f}	
	| e = INT 	; DIVIDE 	; f = INT {print_int(e / f); print_string("\n"); e / f} (* Arithmetic Operators*)

logical_exp:
	| NOT		; e = logical_exp	  	   {not e}
	| e = logical_exp	; LEQ  		; f = logical_exp {e <= f}	
	| e = logical_exp	; GEQ   	; f = logical_exp {e >= f}	
	| e = logical_exp	; EQUAL		; f = logical_exp {e == f }	
	| e = logical_exp	; NOTEQ 	; f = logical_exp {e != f} (* Conditional Operators *)
	| e = logical_exp	; AND 		; f = logical_exp {e && f}	
	| e = logical_exp	; OR   		; f = logical_exp {e || f} (* Logic Operators*)

%%






