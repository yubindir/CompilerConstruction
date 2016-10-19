# CompilerConstruction

## Compiling and Running Tests

Run `make` in the terminal to compile all the necessary files.

Run the script called `runtest` with the command `./runtest`, which will run all tests and give a "Success" vs SyntaxError. The script can be found in the test folder. You may need to run `chmod 755 ./runtest` if you are not permitted to run the script.

### Note on the Tests

Each test program is in one line, so if you would like to view the programs in a normal way, there is another folder called `nice layout` that has each test program in a readable fashion.

## Syntax and Structure of the Code

### Structure

Each program would be a list of functions for example:

main() { ... }

helper() { ... }

...

and each function would be of the form:

functionname ( arguments if any ) { code }

### Syntax
`0`								Numbers are integers (... -1,0,1 ...)

`var varname`						A named variable

`var a = value;`					Assign some value to variable a

`&var a;`							Dereference a variable

`new var a = value/expression;`	New variable a in an expression

`let var a = value/expression;`	A let expression

`fun var f(var a, ...);`			Apply a function to variables (or no variables if its void)

`var a + var b;`					Addition of two variables (or expressions)

`var a - var b;`					Difference of two variables (or expressions)

`var a * var b;`					Product of two variables (or expressions)

`var a / var b;` 					Division of two variables (or expressions)

`var a && var b;`					Logical And of two variables (or expressions)

`var a || var b;`                 Logical Or of two variables (or expressions)

`!var b`							Logical Not of a variable (or expression)

`var a <= var b;`					Compare if one variable (or expression) is less than or equal the other

`var a >= var b;`					Compare if one variable (or expression) is greater than or equal the other

`var a == var b;`					Check equality of two variables (or expressions)

`var a != var b;`					Check inequality of two variables (or expressions)

`while(condition){ code; }`				While condition is true, run the code

`if(condition){ code1; } else { code2; }`				If condition is true, run code1, else run code2
	

	

