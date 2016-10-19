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
	read_line() |>  Lexing.from_string  |> parse_with_error;  
	print_string("Successful Parsing");
	print_newline ();








