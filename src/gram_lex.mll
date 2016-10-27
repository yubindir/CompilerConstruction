{ 
	open Gram_par
	exception SyntaxError of string 
}

let comment_line = "//"([^ '\n' ]+)
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = 
	parse
	| int {INT (int_of_string(Lexing.lexeme lexbuf))}
	| "true" {TRUE (bool_of_string(Lexing.lexeme lexbuf))}
	| "false" {FALSE (bool_of_string(Lexing.lexeme lexbuf))}		
	| "read_int" {READINT}	| "print_int" {PRINTINT}
	| "main" {MAIN}
	| ';' {SEMICOLON}		| ',' {COMMA}
	| '{' {LBRACE}		| '}' {RBRACE}
	| '(' {LBRACKET}	| ')' {RBRACKET}
	| "var" {ID}
	| "let" {LET}		| "new" {NEW} 	| "in" {IN}
	| "fun" {FUN}

	| "while" {WHILE}	
	| "if" {IF}		| "else" {ELSE}
	
	| '+' {PLUS}		| '-' {MINUS}		| '*' {TIMES}		| '/' {DIVIDE}
	| "&&" {AND}		| "||" {OR}		| '!' {NOT}
	| "<=" {LEQ}		| ">=" {GEQ}		| "==" {EQUAL}		| "!=" {NOTEQ}
	| '=' {ASG}		| '&' {DEREF}		
	| id {VARNAME (Lexing.lexeme lexbuf)}
	| comment_line {read lexbuf}
	| white {read lexbuf}
	| newline {read lexbuf}
	| _ {raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
	| eof {EOF} 






















