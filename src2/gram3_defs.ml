type operator = 
	|Plus 	|Minus	|Times	|Divide
	|Leq 	|Geq 	|Equal 	|Noteq
	|And 	|Or

type result =
	| Int of int
	| Bool of bool
	| Str of string
	| Unit of unit

type expression =
	| Id of string
	| Seq of expression * expression
	| Assign of expression * expression

	(*Constants and Value Expressions*)
	| True
	| False
	| Const of int
	| Deref of expression
	| Not of expression
	| Operator of operator * expression * expression
	
	(*Conditional Statements*)
	| While of expression * expression
	| If of expression * expression * expression

	(*Environment Statements*)
	| Let of expression * expression * expression

	(*Input and Output*)
	| Printint of expression
	| Newline

type function_def = string * string list * expression

type program = function_def list





