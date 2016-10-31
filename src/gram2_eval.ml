open Gram_defs

let store = Hashtbl.create 100;;

let extract_int (Intres(i)) = i;;
let extract_bool (Boolres(b)) = b;;
let extract_string (Stringres(s)) = s;;
let extract_unit (Unitres(u)) = u;;

let rec eval_exp = function
	| Id(varname) -> Stringres(varname)
	| Seq(exp1,exp2) -> let _ = eval_exp exp1 in
				  let result = eval_exp exp2 in result
	| Assign(var, exp) -> Unitres(Hashtbl.add store (extract_string(eval_exp var)) (eval_exp exp))
	| Deref(var) -> Hashtbl.find store (extract_string(eval_exp var))

	| True -> Boolres(true)
	| False -> Boolres(false)
	| Const(num) -> Intres(num)

	| Operator(Plus, exp1, exp2) -> Intres(extract_int(eval_exp exp1) + extract_int(eval_exp exp2))
	| Operator(Minus, exp1, exp2) -> Intres(extract_int(eval_exp exp1) - extract_int(eval_exp exp2))
	| Operator(Times, exp1, exp2) -> Intres(extract_int(eval_exp exp1) * extract_int(eval_exp exp2))
	| Operator(Divide, exp1, exp2) -> Intres(extract_int(eval_exp exp1) / extract_int(eval_exp exp2))
	| Not exp -> Boolres(not (extract_bool(eval_exp exp)))
	| Operator(And, exp1, exp2) -> Boolres(extract_bool(eval_exp exp1) && extract_bool(eval_exp exp2))
	| Operator(Or, exp1, exp2) -> Boolres(extract_bool(eval_exp exp1) || extract_bool(eval_exp exp2))
	| Operator(Leq, exp1, exp2) -> Boolres(extract_int(eval_exp exp1) <= extract_int(eval_exp exp2))
	| Operator(Geq, exp1, exp2) -> Boolres(extract_int(eval_exp exp1) >= extract_int(eval_exp exp2))
	| Operator(Equal, exp1, exp2) -> Boolres(extract_int(eval_exp exp1) = extract_int(eval_exp exp2))
	| Operator(Noteq, exp1, exp2) -> Boolres(extract_int(eval_exp exp1) != extract_int(eval_exp exp2))
	| Printint(exp) -> Unitres(print_int(extract_int(eval_exp exp)); print_string("\n"))
	
	| Else(cond, exp1, exp2) -> if extract_bool(eval_exp cond) then eval_exp exp1 else eval_exp exp2
	| If(cond, exp) -> if extract_bool(eval_exp cond) then eval_exp exp else Unitres(())
	| While(cond, exp) -> Unitres(while extract_bool(eval_exp cond) do eval_exp exp done)

let rec eval_other = function
	| Let(v, e, c) -> failwith "Not implemented"
	| New(v, e, c) -> failwith "Not implemented"
	| Readint -> failwith "Not implemented"
	| Apply(v, args) -> failwith "Not implemented";;

let eval_func = function
	| ("main", args, exp) -> eval_exp exp
	| (name, args, exp) -> Unitres(print_string("Other Function found \n"));;

let rec eval_prog = function
	| [] -> print_string("End Of File")
	| (x::xs) -> let _ = eval_func x in 
				 let program = eval_prog xs in program;;
