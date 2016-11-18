open Gram3_defs
open Gram3_helper
open Printf

(* Store for other variables *)
let store = Hashtbl.create 20;;

let rec eval_exp env = function
	| Id(e1) -> Str(e1)
	| Seq(e1,e2) -> let _ = eval_exp env e1 in eval_exp env e2

	(*CONSTANTS*)
	| Const(i) -> Int(i)
	| True -> Bool(true)
	| False -> Bool(false)

	(*OPERATIONS*)
	(*Arithmetic*)
	| Operator(Plus,e1,e2) -> Int(getInt(eval_exp env e1)+getInt(eval_exp env e2))
	| Operator(Minus,e1,e2) -> Int(getInt(eval_exp env e1)-getInt(eval_exp env e2))
	| Operator(Times,e1,e2) -> Int(getInt(eval_exp env e1)*getInt(eval_exp env e2))
	| Operator(Divide,e1,e2) -> Int(getInt(eval_exp env e1)/getInt(eval_exp env e2))

	(*Logic*)
	| Operator(And,e1,e2) -> Bool(getBool(eval_exp env e1)&&getBool(eval_exp env e2))
	| Operator(Or,e1,e2) -> Bool(getBool(eval_exp env e1)||getBool(eval_exp env e2))
	| Not(e1) -> Bool(not (getBool(eval_exp env e1)))

	(*Comparison*)
	| Operator(Leq,e1,e2) -> Bool(lessThan (eval_exp env e1) (eval_exp env e2))
	| Operator(Geq,e1,e2) -> Bool(greaterThan (eval_exp env e1) (eval_exp env e2))
	| Operator(Equal,e1,e2) -> Bool(equal (eval_exp env e1) (eval_exp env e2))
	| Operator(Noteq,e1,e2) -> Bool(notEqual (eval_exp env e1) (eval_exp env e2))

	(*Conditional Statements*)
	| If(e1,e2,e3) ->
		if getBool(eval_exp env e1) then eval_exp env e2
		else eval_exp env e3
	| While(e1, e2) ->
		Unit(while getBool(eval_exp env e1) 
		do eval_exp env e2 done)

	(*Environment Statements*)
	| Assign(e1,e2) ->
		let x = getStr(eval_exp env e1) in
		let v = eval_exp env e2 in
		Hashtbl.add store x v; Unit(())
	| Deref(e1) -> 
		let x = getStr(eval_exp env e1) in
		let v = lookfor env x in
		if v = Unit(()) then Hashtbl.find store x 
		else v
	| Let(e1,e2,e3) ->
		let x = getStr(eval_exp env e1) in
		let v = eval_exp env e2 in
		let env2 = (x,v)::env in
		eval_exp env2 e3

	(*Printing*)
	| Printint(e1) -> Unit(print_int(getInt(eval_exp env e1)))
	| Newline -> Unit(printf("\n"));;

let eval_func = function
	| ("main", args, code) -> eval_exp [] code
	| (name, args, code) -> Unit(printf("Other Function found \n"));;

let rec eval_prog = function
	| [] -> printf("\nEnd Of File\n") 
	| (x::xs) -> let _ = eval_func x in eval_prog xs;;
