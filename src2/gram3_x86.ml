open Gram3_defs
open Gram3_helper
open Printf

let string_of_operator op = match op with
	| Plus -> "add "
	| Minus -> "sub "
	| Times -> "mul "
	| Divide -> "div "

(*Config*)
open Hashtbl;;

let addr_gbl = ref 0;;
let new_addr() = addr_gbl := !addr_gbl+1; !addr_gbl;;

let sp = ref 0;;

(*Instruction Compilation*)
let gen_code = Buffer.create 100

let x86_op op =
	"popq %rax \n" ^
	"popq %rbx \n" ^
	(string_of_operator op) ^ " %rax, %rbx \n" ^
	"pushq %rbx \n" |> Buffer.add_string gen_code

let x86_id addr =
	"//offset " ^ (string_of_int addr) ^ "\n" ^
	"movq " ^ (-16 -4 * addr |> string_of_int) ^ "(%rbp), %rax\n" ^
	"pushq %rax\n" |> Buffer.add_string gen_code

let x86_st n =
	"pushq $" ^ (string_of_int n) ^ "\n" |> Buffer.add_string gen_code

let x86_let _ =
	"popq %rax\n" ^
	"popq %rbx\n" ^
	"push %rax\n" |> Buffer.add_string gen_code

(* Store for other variables *)
let store = Hashtbl.create 20;;

let rec codegen symt = function
	| Id(e1) -> Str(e1)
	| Seq(e1,e2) -> 
		codegen symt e1;
		codegen symt e2

	(*CONSTANTS*)
	| Const(i) ->
		x86_st i;
		sp := !sp+1; Int(!sp)

	(*OPERATIONS*)
	(*Arithmetic*)
	| Operator(Plus,e1,e2) -> 
		codegen symt e1;
		codegen symt e2;
		x86_op Plus;
		sp := !sp+1; Int(!sp)

	| Operator(Minus,e1,e2) ->
		codegen symt e1;
		codegen symt e2;
		x86_op Minus;
		sp := !sp+1; Int(!sp)

	| Operator(Times,e1,e2) -> 
		codegen symt e1;
		codegen symt e2;
		x86_op Times;
		sp := !sp+1; Int(!sp)

	| Operator(Divide,e1,e2) -> 
		codegen symt e1;
		codegen symt e2;
		x86_op Divide;
		sp := !sp+1; Int(!sp)

	(*Environment Statements*)
	| Assign(e1,e2) ->
		let x = getStr(codegen symt e1) in
		let v = codegen symt e2 in
		Hashtbl.add store x v; Unit(())

	| Deref(e1) -> 
		let x = getStr(codegen symt e1) in
		let v = lookfor symt x in
		if v = Unit(()) then Hashtbl.find store x 
		else v

	| Let(e1,e2,e3) ->
		let x = getStr(codegen symt e1) in
		let v = codegen symt e2 in
		let symt2 = (x,v)::symt in
		codegen symt2 e3

	(*Printing*)
	| Printint(e1) ->
		let addr = getInt(codegen symt e1) in
		print_int(addr); Unit(())
	| Newline -> Unit(printf("\n"));;

let codegen_func = function
	| ("main", args, code) -> 
		Buffer.reset gen_code; codegen [] code;
		Unit(())
	| (name, args, code) -> Unit(printf("Other Function found \n"));;

let rec codegen_prog = function
	| [] -> printf("\nEnd Of File\n") 
	| (x::xs) -> let _ = codegen_func x in codegen_prog xs; Buffer.output_buffer stdout gen_code;;
