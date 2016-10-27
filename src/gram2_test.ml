open Gram_lex
open Gram_eval
open Lexing 
open Printf
open String

let print_position lexbuf =   
	let pos = lexbuf.lex_curr_p in   
	eprintf "Pos %d:%d:%d\n" pos.pos_lnum pos.pos_bol pos.pos_cnum 

let parse_with_error lexbuf =   
	try Gram_par.program Gram_lex.read lexbuf with
	| SyntaxError msg -> prerr_string (msg ^ ": "); 
						 print_position lexbuf;                        
						 exit (-1) 
	| Gram_par.Error ->  prerr_string "Parse error: ";
                         print_position lexbuf;
                         exit (-1)

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let _ = read_line() |> String.trim |> load_file |> Lexing.from_string  |> parse_with_error |> eval_prog;
	print_newline ();








