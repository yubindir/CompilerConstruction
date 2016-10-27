type operator = 
	|Plus |Minus |Times |Divide
	|Leq |Geq |Equal |Noteq
	|And |Or

type variable =
	| Id of string

type expression =
	| Seq of expression * expression
	| Assign of variable * expression

	(*Valuee and Value Expressions*)
	| True
	| False
	| Const of int
	| Deref of variable
	| Not of expression
	| Operator of operator * expression * expression
	
	(*Conditional Statements*)
	| While of expression * expression
	| If of expression * expression
	| Else of expression * expression * expression

	(*Environment Statements*)
	| Let of variable * expression * expression
	| New of variable * expression * expression

	(*Input and Output*)
	| Readint
	| Printint of expression

	(*Function Application*)
	| Apply of variable * expression list
	

type function_def = string * variable list * expression

type program = function_def list
