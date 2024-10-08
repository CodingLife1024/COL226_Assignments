Consider the language you used for Assignment 6 or some extension thereof, and design and implement (in OCaml) the Krivine machine that implements Call-by-Name semantics. 

As usual, make whatever design choices you deem fit, and provide test cases which demonstrate that your implementations of the machine runs correctly.


Here is the specification for Assignment 6. The due date is March 25, 2024. Please submit on Gradescope.

----------

Consider the language of the lambda calculus, which is as follows:

e ::= x | \x. e_1 | (e_1 e_2)

One can extend this language with other constructs we have seen in this course, namely numerals, booleans, tuples, projections, conditionals, case statements etc. Consider some such extension, and design and implement (in OCaml) the SECD machine that implements Call-by-Value semantics. For this you need value closures in the set of answers. You also need to implement the compile function.

As usual, make whatever design choices you deem fit, and provide test cases which demonstrate that your implementations of the machine runs correctly.