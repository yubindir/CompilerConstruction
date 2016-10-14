open Gram_lex 
open Lexing 
open Printf

let print_position lexbuf =   
	let pos = lexbuf.lex_curr_p in   
	eprintf "Pos %d:%d:%d\n" pos.pos_lnum pos.pos_bol pos.pos_cnum 

let parse_with_error lexbuf =   
	try Gram_par.start Gram_lex.read lexbuf with
	| SyntaxError msg -> prerr_string (msg ^ ": "); 
						 print_position lexbuf;                        
						 exit (-1) 
	| Gram_par.Error ->  prerr_string "Parse error: ";
                         print_position lexbuf;
                         exit (-1) 


let _ = 
	read_line()|> Lexing.from_string  |> parse_with_error;  
	print_string("Successful Parsing");
	print_newline ();

(*
Code for the demonstration

bisection(var f, var a, var b, var TOL, var NMAX) 
{ 
	new var n = 1; 
	while (var n <= var NMAX) 
	{ 
		var c = (var a + var b)/2; 
		if ((fun var f (var c) == 0) || ( (var b - var a)/2 <= var TOL ) ) 
		{ 
			return var c 
		}; 
		var n = var n + 1; 
		if (fun var sign(var c) == fun var sign(var c)) 
		{ 
			var a = var c; 
		} 
		else 
		{ 
			var b = var c; 
		} 
	} 
} 

quadratic(var c) 
{ 
	return (var c * var c) + var c + 1; 
} 

main() 
{ 
	let var TOL = 10; 
	let var NMAX = 100; 
	new var a = 1; 
	new var b = 1; 
	new var root = fun var bisection(var quadratic, var a, var b, var TOL, var NMAX); 
	return var root; 
}

*)

















