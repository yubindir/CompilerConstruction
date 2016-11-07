open Gram_defs
open Gram2_eval

let rec opt_exp env = function
	| Const(num) -> Const(num)
	| True -> True
	| False -> False
	| Id(varname) -> Id(varname)
	| Seq(exp1,exp2) -> let _ = opt_exp env exp1 in
				  		let result = opt_exp env exp2 in result
	| Assign(var, exp) -> Assign(var, (opt_exp env exp))
	| Deref(var) -> Deref(var)
	| Operator(Plus, exp1, exp2) -> Const(extract_int(eval_exp env exp1) + extract_int(eval_exp env exp2))
	| Operator(Minus, exp1, exp2) -> Const(extract_int(eval_exp env exp1) - extract_int(eval_exp env exp2))
	| Operator(Times, exp1, exp2) -> Const(extract_int(eval_exp env exp1) * extract_int(eval_exp env exp2))
	| Operator(Divide, exp1, exp2) -> Const(extract_int(eval_exp env exp1) / extract_int(eval_exp env exp2))
	| Not exp -> Not(if extract_bool(eval_exp env exp) then True else False)
	| Operator(And, exp1, exp2) -> if (extract_bool(eval_exp env exp1) && extract_bool(eval_exp env exp2)) then True else False
	| Operator(Or, exp1, exp2) -> if (extract_bool(eval_exp env exp1) || extract_bool(eval_exp env exp2)) then True else False
	| Operator(Leq, exp1, exp2) -> if (extract_int(eval_exp env exp1) <= extract_int(eval_exp env exp2)) then True else False
	| Operator(Geq, exp1, exp2) -> if (extract_int(eval_exp env exp1) >= extract_int(eval_exp env exp2)) then True else False
	| Operator(Equal, exp1, exp2) -> if (extract_int(eval_exp env exp1) = extract_int(eval_exp env exp2)) then True else False
	| Operator(Noteq, exp1, exp2) -> if (extract_int(eval_exp env exp1) != extract_int(eval_exp env exp2)) then True else False
	| Printint(exp) -> Printint(opt_exp env exp)
	| If(cond, exp) -> If(cond, exp)
	| Else(cond, exp1, exp2) -> if extract_bool(eval_exp env cond) then opt_exp env exp1 else opt_exp env exp2

	| While(cond, exp) -> While(cond, exp)
	| Let(var, exp1, exp2) -> Let(var, exp1, exp2);;

let opt_func = function
	| ("main", args, exp) -> ("main", args, (opt_exp [] exp))
	| (name, args, exp) -> (name, args, exp);;

(*let rec opt_prog = function
	| [] -> print_string("End Of File")
	| (x::xs) -> let _ = opt_func x in 
				 let program = opt_prog xs in program;;*)

let rec opt_prog = function
	| [] -> []
	| (x::xs) -> let func = opt_func x in
				 let program = opt_prog xs in
				 clear_step(); func :: program;;









