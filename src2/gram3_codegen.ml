open Gram3_defs
open Gram3_helper
open Printf

(*Config*)
open Hashtbl;;

let addr_gbl = ref 0;;
let new_addr() = addr_gbl := !addr_gbl+1; !addr_gbl;;

(*Instruction Compilation*)
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

let rec codegen symt = function
	| Id(e1) -> Str(e1)
	| Seq(e1,e2) -> let _ = codegen symt e1 in codegen symt e2

	(*CONSTANTS*)
	| Const(i) ->
		let addr = new_addr() in
		codegen_ldc i; codegen_st addr; Int(addr)
	| True ->
		let addr = new_addr() in
		codegen_ldc 1; codegen_st addr; Int(addr)
	| False ->
		let addr = new_addr() in
		codegen_ldc 0; codegen_st addr; Int(addr)

	(*OPERATIONS*)
	(*Arithmetic*)
	(*Sorry for Ugly Code*)
	| Operator(Plus,e1,e2) -> 
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		codegen_op(Plus, getInt(addr1), getInt(addr2));
		let addr3 = new_addr() in
		codegen_st addr3; 
		"ld r" ^ (string_of_int (addr3))
		^ "\n" |> Buffer.add_string gen_code; Int(addr3)

	| Operator(Minus,e1,e2) ->
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		codegen_op(Minus, getInt(addr1), getInt(addr2));
		let addr3 = new_addr() in
		codegen_st addr3; 
		"ld r" ^ (string_of_int (addr3))
		^ "\n" |> Buffer.add_string gen_code; Int(addr3)

	| Operator(Times,e1,e2) -> 
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		codegen_op(Times, getInt(addr1), getInt(addr2));
		let addr3 = new_addr() in
		codegen_st addr3; 
		"ld r" ^ (string_of_int (addr3))
		^ "\n" |> Buffer.add_string gen_code; Int(addr3)

	| Operator(Divide,e1,e2) -> 
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		codegen_op(Divide, getInt(addr1), getInt(addr2));
		let addr3 = new_addr() in
		codegen_st addr3; 
		"ld r" ^ (string_of_int (addr3))
		^ "\n" |> Buffer.add_string gen_code; Int(addr3)

	(*
	Problem with the boolean operations is that they are cultivated to work for IF statements only.
	*)

	(*Comparison*)
	| Operator(Leq,e1,e2) ->
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		(print_cmp addr1 addr2) |> Buffer.add_string gen_code; 
		Int(1)

	| Operator(Geq,e1,e2) ->
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		(print_cmp addr1 addr2) |> Buffer.add_string gen_code;
		Int(2)

	| Operator(Equal,e1,e2) ->
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		(print_cmp addr1 addr2) |> Buffer.add_string gen_code;
		Int(3)

	| Operator(Noteq,e1,e2) ->
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		(print_cmp addr1 addr2) |> Buffer.add_string gen_code;
		Int(4)

	(*Logic*)
	| Operator(And,e1,e2) ->
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		(print_and addr1 addr2) |> Buffer.add_string gen_code;
		Int(5)

	| Operator(Or,e1,e2) ->
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		(print_or addr1 addr2) |> Buffer.add_string gen_code;
		Int(5)

	| Not(e1) ->
		let addr1 = codegen symt e1 in
		"ld r" ^ (string_of_int (getInt(addr1)))
		^ "\ncmp r" ^ (string_of_int (getInt(addr1)))
		^ " 1 \njne ELSE \n" |> Buffer.add_string gen_code;
		Int(5)

	(*Conditional Statements*)
	| If(e1,e2,e3) ->
		let exp = getInt(codegen symt e1) in
		let op = (write_op exp) in 
		op |> Buffer.add_string gen_code; 
		codegen symt e2;
		"j CODE\n" ^ "ELSE: \n" |> Buffer.add_string gen_code;
		codegen symt e3;
		"CODE: \n" |> Buffer.add_string gen_code; Unit(())

	| While(e1,e2) -> failwith "Not Implemented"

		(*
		While Loop
		1. CMP label
		2. Comparison check
		3. Jump to CODE if comparison fails
		3. LOOP label
		4. Loop body
		5. Jump to CMP
		*)

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
		"call print \n" |> Buffer.add_string gen_code;
		Unit(())
	| Newline -> 
		"call newline \n" |> Buffer.add_string gen_code;
		Unit(());;

let codegen_func = function
	| ("main", args, code) -> 
		Buffer.reset gen_code; codegen [] code;
		Unit(())
	| (name, args, code) -> Unit(printf("Other Function found \n"));;

let rec codegen_prog = function
	| [] -> printf("\nEnd Of File\n") 
	| (x::xs) -> let _ = codegen_func x in codegen_prog xs; Buffer.output_buffer stdout gen_code;;
