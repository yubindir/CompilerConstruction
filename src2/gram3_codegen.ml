open Gram3_defs
open Gram3_helper
open Printf

(*Config*)
open Hashtbl;;
let ram = Hashtbl.create 100;;
let acc = ref 0;;

let addr_gbl = ref 0;;
let new_addr() = addr_gbl := !addr_gbl+1; !addr_gbl;;

(*Instruction Execution*)
let op (op, addr1, addr2) =
	acc := op (find ram addr1) (find ram addr2);;
let st addr = replace ram addr !acc;;
let ldc n = acc := n;;

(*Instruction Execution*)
let gen_code = Buffer.create 100
let codegen_op (op, addr1, addr2) =
	(string_of_operator op)
	^ " r" ^ (string_of_int addr1)
	^ ", r" ^ (string_of_int addr2)
	^ "\n" |> Buffer.add_string gen_code
let codegen_st addr =
	"st r" ^ (string_of_int addr)
	^ "\n" |> Buffer.add_string gen_code
let codegen_ldc n = 
	"ld " ^ (string_of_int n)
	^ "\n" |> Buffer.add_string gen_code

(* Store for other variables *)
let store = Hashtbl.create 20;;

let rec interpret symt = function
	| Id(e1) -> Str(e1)
	| Seq(e1,e2) -> let _ = interpret symt e1 in interpret symt e2

	(*CONSTANTS*)
	| Const(i) ->
		let addr = new_addr() in
		ldc i; st addr; Int(addr)

	(*OPERATIONS*)
	(*Arithmetic*)
	| Operator(Plus,e1,e2) -> 
		let addr1 = interpret symt e1 in
		let addr2 = interpret symt e2 in
		op((+), getInt(addr1), getInt(addr2));
		let addr3 = new_addr() in
		st addr3; Int(addr3)
	| Operator(Minus,e1,e2) ->
		let addr1 = interpret symt e1 in
		let addr2 = interpret symt e2 in
		op((-), getInt(addr1), getInt(addr2));
		let addr3 = new_addr() in
		st addr3; Int(addr3)
	| Operator(Times,e1,e2) -> 
		let addr1 = interpret symt e1 in
		let addr2 = interpret symt e2 in
		op(( * ), getInt(addr1), getInt(addr2));
		let addr3 = new_addr() in
		st addr3; Int(addr3)
	| Operator(Divide,e1,e2) -> 
		let addr1 = interpret symt e1 in
		let addr2 = interpret symt e2 in
		op((/), getInt(addr1), getInt(addr2));
		let addr3 = new_addr() in
		st addr3; Int(addr3)

	(*Environment Statements*)
	| Assign(e1,e2) ->
		let x = getStr(interpret symt e1) in
		let v = interpret symt e2 in
		Hashtbl.add store x v; Unit(())
	| Deref(e1) -> 
		let x = getStr(interpret symt e1) in
		let v = lookfor symt x in
		if v = Unit(()) then Hashtbl.find store x 
		else v
	| Let(e1,e2,e3) ->
		let x = getStr(interpret symt e1) in
		let v = interpret symt e2 in
		let symt2 = (x,v)::symt in
		interpret symt2 e3

	(*Printing*)
	| Printint(e1) ->
		let addr = getInt(interpret symt e1) in
		let v = find ram addr in
		print_int(addr); Unit(())
	| Newline -> Unit(printf("\n"));;

let eval_func = function
	| ("main", args, code) -> interpret [] code
	| (name, args, code) -> Unit(printf("Other Function found \n"));;

let rec eval_prog = function
	| [] -> printf("\nEnd Of File\n") 
	| (x::xs) -> let _ = eval_func x in eval_prog xs;;
