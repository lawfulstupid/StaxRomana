# StaxRomana
Stack-based roman numeral manipulation language.

## Arithmetic Commands
	I	Push 1 to stack.
	V	Push 5 to stack.
	X	Push 10 to stack.
	L	Push 50 to stack.
	C	Push 100 to stack.
	D	Push 500 to stack.
	M	Push 1000 to stack.
	+	Pop 2, push sum.
	-	Pop 2, push 2nd-1st (e.g. X I - ~> [9]).
	*	Pop 2, push product.
	/	Pop 2, push 2nd/1st (integer division) (e.g. X V / ~> [2]).
	%	Pop 2, push 2nd%1st (modulo) (e.g. XV VI % ~> [3])
	S	Pop stack, push sum.
	P	Pop stack, push product.
### Arithmetic Sequences
	I¬	Push 0 to stack.

## Comparator Commands
	=	Pop 2, push (1st == 2nd).
	!	Pop 2, push (1st != 2nd).
	<	Pop 2, push 1st<2nd (e.g. X V < ~> [1]).
	>	Pop 2, push 1st>2nd (e.g. X V > ~> [0]).
### Comparator Sequences
	?=;<|	Pop 2, push 1st<=2nd (e.g. X V ?=;<| ~> [1]).
	?=;>|	Pop 2, push 1st>=2nd.

## Logic Commands
	Treats zero as False, non-zero as True, False as zero, True as 1.
	¬	Replace head with inversion.
	&	Pop 2, push conjunction.
	|	Pop 2, push disjunction.
### Logic Sequences
	¬¬$¬¬=	Logical equivalency.
	¬¬$¬¬!	XOR.

## Stack Manipulation
	d	Duplicate head.
	?	Push 2nd, 1st.
	;	Move head 2 spaces back.
	$	Swap top 2 elements.
	r	Reverse stack.
	.	Pop 1.
	_	Clear stack (pop all).

## I/O
	#	Pop 1, print number.
	~	Pop stack, print as numbers.
	'	Pop 1, print character.
	"	Pop stack, print as characters.
	,	Read char from stdin
	@	Read line from stdin

## Control
	(	If condition: If head is zero, jump to after corresponding )
	)	No effect
	{	While loop: If head is zero, jump to after corresponding }
	}	Jump to corresponding {
	[	No effect
	]	Repeat loop: If head is non-zero, jump to corresponding [
	
## Comments
	`	Block comment until next backtick.


	
# Examples
d+		Double head.
dd++	Triple head.
d*		Square head.
d**		Cube head.
d-		Replace head with 0.
?=(...)	If 1st==2nd, do something.
?!(...)	If 1st!=2nd, do something.
?&(...)	If 1st and 2nd, do something.
¬(¬...)	If head is zero, do something.
?>{.$d;I+$?>}.?!(.)..	Pop 2, push sequence from 1st to 2nd (e.g. VX: ~> [5,6,7,8,9,10])


## Hello world
LXXII CI CVIII d CXI XLIV XXXII LXXXVII CXI CXIV CVIII C XXXIII "