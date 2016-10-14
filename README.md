# CompilerConstruction

## Compiling and Running Tests

There is a file called `makefile-1` in the test directory which you can run in your terminal as `./makefile-1`
Apologies in advance for not setting up a proper make file

In the test folder there is a script called `./runtest`
This would run all the tests and give a "Success" vs SyntaxError.

You may need to run `chmod 755 ./makefile-1` and `chmod 755 ./runtest` if you are not permitted to run these files.

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
	

	

