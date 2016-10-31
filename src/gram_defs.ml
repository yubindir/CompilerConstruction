type operator = 
	|Plus |Minus |Times |Divide
	|Leq |Geq |Equal |Noteq
	|And |Or

type result =
	| Intres of int
	| Boolres of bool
	| Stringres of string
	| Unitres of unit

type expression =
	| Id of string
	| Seq of expression * expression
	| Assign of expression * expression

	(*Valuee and Value Expressions*)
	| True
	| False
	| Const of int
	| Deref of expression
	| Not of expression
	| Operator of operator * expression * expression
	
	(*Conditional Statements*)
	| While of expression * expression
	| If of expression * expression
	| Else of expression * expression * expression

	(*Environment Statements*)
	| Let of expression * expression * expression
	| New of expression * expression * expression

	(*Input and Output*)
	| Readint
	| Printint of expression

	(*Function Application*)
	| Apply of expression * expression list	

type function_def = string * expression list * expression

type program = function_def list
