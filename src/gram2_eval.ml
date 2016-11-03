open Gram_defs

let store = Hashtbl.create 100;;

let extract_int (Intres(i)) = i;;
let extract_bool (Boolres(b)) = b;;
let extract_string (Stringres(s)) = s;;
let extract_unit (Unitres(u)) = u;;
let extract_list (Listres(l)) = l;;

let rec removefrom x = function
	| [] -> Unitres(())
	| (elem, value) :: env -> if x = elem then value else removefrom x env;;

let rec printenv = function
	| [] -> print_string("\n")
	| e::es -> print_string(fst(e) ^ "\n"); printenv es;;

let rec eval_exp env = function
	| Id(varname) -> Stringres(varname)
	| Seq(exp1,exp2) -> let _ = eval_exp env exp1 in
				  let result = eval_exp env exp2 in result
	| Assign(var, exp) -> Unitres(Hashtbl.add store (extract_string(eval_exp env var)) (eval_exp env exp))
(*	| Assign(var, exp) -> let v = extract_string(eval_exp env var) in
						  let e = eval_exp env exp in
						  let envlist = (v,e) :: env in
						  print_string("Adding to env \n");
						  printenv envlist;
						  Listres(envlist) *)
(*	| Deref(var) -> Hashtbl.find store (extract_string(eval_exp env var)) *)
	| Deref(var) -> let v = extract_string(eval_exp env var) in
					if removefrom v env = Unitres(()) then Hashtbl.find store (extract_string(eval_exp env var)) else
					removefrom v env

	| True -> Boolres(true)
	| False -> Boolres(false)
	| Const(num) -> Intres(num)

	| Operator(Plus, exp1, exp2) -> Intres(extract_int(eval_exp env exp1) + extract_int(eval_exp env exp2))
	| Operator(Minus, exp1, exp2) -> Intres(extract_int(eval_exp env exp1) - extract_int(eval_exp env exp2))
	| Operator(Times, exp1, exp2) -> Intres(extract_int(eval_exp env exp1) * extract_int(eval_exp env exp2))
	| Operator(Divide, exp1, exp2) -> Intres(extract_int(eval_exp env exp1) / extract_int(eval_exp env exp2))
	| Not exp -> Boolres(not (extract_bool(eval_exp env exp)))
	| Operator(And, exp1, exp2) -> Boolres(extract_bool(eval_exp env exp1) && extract_bool(eval_exp env exp2))
	| Operator(Or, exp1, exp2) -> Boolres(extract_bool(eval_exp env exp1) || extract_bool(eval_exp env exp2))
	| Operator(Leq, exp1, exp2) -> Boolres(extract_int(eval_exp env exp1) <= extract_int(eval_exp env exp2))
	| Operator(Geq, exp1, exp2) -> Boolres(extract_int(eval_exp env exp1) >= extract_int(eval_exp env exp2))
	| Operator(Equal, exp1, exp2) -> Boolres(extract_int(eval_exp env exp1) = extract_int(eval_exp env exp2))
	| Operator(Noteq, exp1, exp2) -> Boolres(extract_int(eval_exp env exp1) != extract_int(eval_exp env exp2))
	| Printint(exp) -> Unitres(print_int(extract_int(eval_exp env exp)); print_string("\n"))
	
	| Else(cond, exp1, exp2) -> if extract_bool(eval_exp env cond) then eval_exp env exp1 else eval_exp env exp2
	| If(cond, exp) -> if extract_bool(eval_exp env cond) then eval_exp env exp else Unitres(())
	| While(cond, exp) -> Unitres(while extract_bool(eval_exp env cond) do eval_exp env exp done)

	| Let(var, exp1, exp2) -> let v = eval_exp env exp1 in
							  eval_exp (((extract_string(eval_exp env var)), v)::env) exp2;;

let rec eval_other = function
	| New(v, e, c) -> failwith "Not implemented"
	| Readint -> failwith "Not implemented"
	| Apply(v, args) -> failwith "Not implemented";;

let eval_func = function
	| ("main", args, exp) -> eval_exp [] exp
	| (name, args, exp) -> Unitres(print_string("Other Function found \n"));;

let rec eval_prog = function
	| [] -> print_string("End Of File")
	| (x::xs) -> let _ = eval_func x in 
				 let program = eval_prog xs in program;;
