{ 
	open Gram3_par
	exception SyntaxError of string 
}

let comment_line = "//"([^ '\n' ]+)
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = 
	parse
	| int {INT (int_of_string(Lexing.lexeme lexbuf))}
	| "true" {TRUE (bool_of_string(Lexing.lexeme lexbuf))}
	| "false" {FALSE (bool_of_string(Lexing.lexeme lexbuf))}		
	| "print_int" {PRINTINT} | "newline" {NEWLINE}
	| "main" {MAIN}
	| ';' {SEMICOLON}	| ',' {COMMA}
	| '{' {LCB}		| '}' {RCB}
	| '(' {LRB}		| ')' {RRB}
	| "var" {TYPE}
	| "let" {LET}		| "in" {IN}

	| "while" {WHILE}	
	| "if" {IF}		| "else" {ELSE}
	
	| '+' {PLUS}		| '-' {MINUS}		| '*' {TIMES}		| '/' {DIVIDE}
	| "<=" {LEQ}		| ">=" {GEQ}		| "==" {EQUAL}		| "!=" {NOTEQ}
	| "&&" {AND}		| "||" {OR}		| '!' {NOT}
	| '=' {ASG}		| '&' {DEREF}		
	| id {NAME (Lexing.lexeme lexbuf)}
	| comment_line {read lexbuf}
	| white {read lexbuf}
	| newline {read lexbuf}
	| _ {raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
	| eof {EOF} 






















