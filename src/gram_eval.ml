open Gram_defs

let eval_var = function
	| Id(name) -> print_string("Id(" ^ name ^ ") \n");;

let eval_op = function
	| Plus -> print_string("Plus found \n")
	| Minus -> print_string("Minus found \n")
	| Times -> print_string("Times found \n")
	| Divide -> print_string("Divide found \n")
	| Leq -> print_string("Leq found \n")
	| Geq -> print_string("Geq found \n")
	| Equal -> print_string("Equal found \n")
	| Noteq -> print_string("Noteq found \n")
	| And -> print_string("And found \n")
	| Or -> print_string("Or found \n")


let rec eval_exp = function
	| Seq(e,f) -> print_string("Sequence found \n"); eval_exp e; eval_exp f;
	| Assign(v,f) -> print_string("Assign found \n"); eval_var v; eval_exp f;
	
	(*Valuee and Value Expressions*)
	| True -> print_string("Truth found \n")
	| False -> print_string("Lies found \n")
	| Const(i) -> print_string("Number found \n"); print_int(i); print_string("\n")
	| Deref(v) -> print_string("Dereference found \n"); eval_var v
	| Not(e) -> print_string("Not found \n"); eval_exp e

	(*Need to split it*)
	| Operator(op, e, f) -> print_string("Operation "); eval_op op; eval_exp e; eval_exp f

	(*Conditional Statements*)
	| While(e,c) -> print_string("While found \n"); eval_exp e; eval_exp c
	| If(e,c) -> print_string("If found \n"); eval_exp e; eval_exp c
	| Else(e, c, d) -> print_string("If Else found \n"); eval_exp e; eval_exp c; eval_exp d
	
	(*Environment Statements*)
	| Let(v, e, c) -> print_string("Let found \n"); eval_var v; eval_exp e; eval_exp c
	| New(v, e, c) -> print_string("New found \n"); eval_var v; eval_exp e; eval_exp c
	
	(*Input and Output*)
	| Readint -> print_string("Readint found \n")
	| Printint(e) -> print_string("Printint found \n"); eval_exp e;

	(*Function Application*)
	| Apply(v, args) -> print_string("Function application found\n"); eval_var v;;

let eval_func = function
	| ("main", args, c) -> eval_exp c
	| (name, args, c) -> print_string("Other Function found \n");;

let rec eval_prog = function
	| [] -> print_string("Nothing here \n")
	| (x::xs) -> eval_func x; eval_prog xs;;
