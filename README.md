# StaxRomana
Stack-based roman numeral manipulation language.

## Commands
The most basic command is pushing a number to the stack. You do this by writing a valid Roman numeral. Please note that Romans numerals cannot be negative.
### Arithmetic
	+	Pop 2, push sum.
	-	Pop 2, push 2nd-1st (e.g. X I - ~> [9]).
	*	Pop 2, push product.
	/	Pop 2, push 2nd/1st (integer division) (e.g. X V / ~> [2]).
	%	Pop 2, push 2nd%1st (modulo) (e.g. XV VI % ~> [3])
	S	Pop stack, push sum.
	P	Pop stack, push product.
### Comparators
	=	Pop 2, push (1st == 2nd).
	!	Pop 2, push (1st != 2nd).
	<	Pop 2, push 1st<2nd (e.g. X V < ~> [1]).
	>	Pop 2, push 1st>2nd (e.g. X V > ~> [0]).
### Stack Manipulation
	d	Duplicate head.
	?	Push 2nd, 1st.
	;	Move head 2 spaces back.
	:	Move head to bottom of stack.
	$	Swap top 2 elements.
	r	Reverse stack.
	.	Pop 1.
	_	Clear stack (pop all).
### Logic
Treats zero as False, non-zero as True.
	¬	Replace head with inversion.
	&	Pop 2, push conjunction.
	|	Pop 2, push disjunction.
### Control
Stack head is treated as 0 when stack is empty.
	()	If statement: executes once if stack head is non-zero.
	{}	While loop: executes forever while stack head is non-zero.
	[]	Repeat loop: executes once, then repeats while stack head is non-zero.
### I/O
	#	Pop 1, print number.
	~	Pop stack and print it.
	'	Pop 1, print as ASCII character.
	"	Pop stack, print as ASCII characters.
	,	Read character from STDIN and push to stack.
	@	Read line from STDIN and push to stack.
### Comments
	`	Block comment until next backtick.

## Examples
### Arithmetic
	I¬	Push 0 to stack.
	d+	Double head.
	dd++	Triple head.
	d*	Square head.
	d**	Cube head.
### Weak Comparators
	?=;<|	Pop 2, push 1st<=2nd (e.g. X V ?=;<| ~> [1]).
	?=;>|	Pop 2, push 1st>=2nd.
### Additional Logic
	¬¬$¬¬=	Logical equivalency.
	¬¬$¬¬!	XOR.
### Control 
	?=(...)	If 1st==2nd, do something.
	?!(...)	If 1st!=2nd, do something.
	?&(...)	If 1st and 2nd, do something.
	¬(¬...)	If head is zero, do something.
### I/O
	@"		Read & echo
	I¬r{d':}.r	Print stack without clearing
### Miscellaneous
	?>{.$d;I+$?>}.?!(.)..	Pop 2, push sequence from 1st to 2nd (e.g. VX: ~> [5,6,7,8,9,10])
### Hello world
	LXXII CI CVIII d CXI XLIV XXXII LXXXVII CXI CXIV CVIII C XXXIII "
