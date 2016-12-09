open Gram3_defs;;

let getInt (Int(x)) = x;;
let getBool (Bool(x)) = x;;
let getStr (Str(x)) = x;;
let getUnit (Unit(x)) = x;;

(* UGLY  but it works *)
(* Using the comparison operators for both Bools and Ints*)
let lessThan x y =
	let (a,b) = (x,y) in
	match (a,b) with
	| (Bool(e1), Bool(e2)) -> e1 <= e2
	| (Int(e1), Int(e2)) -> e1 <= e2;;
let greaterThan x y =
	let (a,b) = (x,y) in
	match (a,b) with
	| (Bool(e1), Bool(e2)) -> e1 >= e2
	| (Int(e1), Int(e2)) -> e1 >= e2;;
let equal x y =
	let (a,b) = (x,y) in
	match (a,b) with
	| (Bool(e1), Bool(e2)) -> e1 == e2
	| (Int(e1), Int(e2)) -> e1 == e2;;
let notEqual x y =
	let (a,b) = (x,y) in
	match (a,b) with
	| (Bool(e1), Bool(e2)) -> e1 != e2
	| (Int(e1), Int(e2)) -> e1 != e2;;

(*For looking in an environment*)
let rec lookfor env x = match env with
	| [] -> Unit(())
	| ((e,v)::xs) -> if x = e then v else lookfor xs x;;


let string_of_operator op = match op with
	| Plus -> "add "
	| Minus -> "sub "
	| Times -> "mul "
	| Divide -> "div "

let print_cmp addr1 addr2=
	"ld r" ^ (string_of_int (getInt(addr1)))
	^ "\nld r" ^ (string_of_int (getInt(addr2)))
	^ "\ncmp r" ^ (string_of_int (getInt(addr1)))
	^ " r" ^ (string_of_int (getInt(addr2)))
	^ "\n";;

(* Prints assembly for AND, only works for IF statements *)
let print_and addr1 addr2=
	"ld r" ^ (string_of_int (getInt(addr1)))
	^ "\nld r" ^ (string_of_int (getInt(addr2)))
	^ "\ncmp r" ^ (string_of_int (getInt(addr1)))
	^ " 0 \njne NEXT \nj ELSE \nNEXT:" 
	^ "\ncmp r" ^ (string_of_int (getInt(addr2)))
	^ " 0 \nje " ^ "ELSE\n";;

(* Prints assembly for OR, only works for IF statements *)
let print_or addr1 addr2=
	"ld r" ^ (string_of_int (getInt(addr1)))
	^ "\nld r" ^ (string_of_int (getInt(addr2)))
	^ "\ncmp r" ^ (string_of_int (getInt(addr1)))
	^ " 0 \nje NEXT \nj IF \nNEXT:" 
	^ "\ncmp r" ^ (string_of_int (getInt(addr2)))
	^ " 0 \nje ELSE \nIF: \n";;


let write_op n = match n with
(*Leq*)		| 1 -> "jg " ^ "ELSE\n"
(*Geq*)		| 2 -> "jl " ^ "ELSE\n"
(*Equal*)	| 3 -> "jnz " ^ "ELSE\n"
(*Noteq*)	| 4 -> "jz " ^ "ELSE\n"
(*Do Nothing*)	| 5 -> ""
