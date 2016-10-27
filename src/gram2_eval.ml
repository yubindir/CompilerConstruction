open Gram_defs

let store = Hashtbl.create 100;;

let rec eval_arithmetic = function
	| Seq(e,f) -> let _ = eval_arithmetic e in let v = eval_arithmetic f in v
	| Const(i) -> i
	| Assign(Id (v),f) -> Hashtbl.replace store v (eval_arithmetic f);0
	| Deref(Id (v)) -> let value = Hashtbl.find store v in (*print statements*) print_int(value); print_string("\n"); value
	| Operator(op, e, f) -> match op with
							|Plus -> eval_arithmetic e + eval_arithmetic f
							|Minus -> eval_arithmetic e - eval_arithmetic f
							|Times -> eval_arithmetic e * eval_arithmetic f
							|Divide -> eval_arithmetic e / eval_arithmetic f;;

let rec eval_logic = function
	| Not(e) -> let value = not (eval_logic e) in value
	| Operator(op, e, f) -> match op with
							|And -> eval_logic e && eval_logic f
							|Or -> eval_logic e || eval_logic f;;

let rec eval_compare = function
	| Operator(op, e, f) -> match op with
							|Leq -> eval_compare e <= eval_compare f
							|Geq -> eval_compare e >= eval_compare f
							|Equal -> eval_compare e == eval_compare f
							|Noteq -> eval_compare e != eval_compare f;;

(*
let rec eval_condition = function
	| While(e,c) -> let boolean = eval_logic e in
					let c' = eval_arithmetic c in
					if boolean then eval_condition(While(e,c))
								else ()
	| If(e,c) -> let boolean = eval_logic e in
				 let c' = eval_arithmetic c in
				 if boolean then c' else ()
	| Else(e,c,d) -> let boolean = eval_logic e in
				   	 let c' = eval_arithmetic c in
					 let d' = eval_arithmetic d in
				   	 if boolean then c' else d';;
*)

let rec eval_other = function
	| Let(v, e, c) -> failwith "Not implemented"
	| New(v, e, c) -> failwith "Not implemented"
	| Readint -> failwith "Not implemented"
	| Printint(e) -> failwith "Not implemented"
	| Apply(v, args) -> failwith "Not implemented";;

let eval_func = function
	| ("main", args, c) -> eval_arithmetic c
	| (name, args, c) -> print_string("Other Function found \n");0;;

let rec eval_prog = function
	| [] -> print_string("End Of File \n")
	| (x::xs) -> eval_func x; eval_prog xs;;
