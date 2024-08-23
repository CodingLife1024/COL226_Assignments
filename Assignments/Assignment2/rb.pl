% Grammar
exp(N) --> number(N).
exp(true) --> [true].
exp(false) --> [false].
exp(X) --> [X], { atom(X) }.
exp(E1 + E2) --> exp(E1), ['+'], exp(E2). %addition
exp(E1 - E2) --> exp(E1), ['-'], exp(E2). %subtraction
exp(E1 * E2) --> exp(E1), ['*'], exp(E2). %multiplication
exp(E1 ∧ E2) --> exp(E1), ['∧'], exp(E2). %and
exp(E1 ∨ E2) --> exp(E1), ['∨'], exp(E2). %or
exp(¬E) --> ['¬'], exp(E). %not
exp(E1 = E2) --> exp(E1), ['='], exp(E2). %equals
exp(E1 > E2) --> exp(E1), ['>'], exp(E2). %greater
exp(E1 < E2) --> exp(E1), ['<'], exp(E2). %less
exp(E1 ** E2) --> exp(E1), ['**'], exp(E2). % power
exp(add(E1, E2)) --> ['add'], exp(E1), exp(E2). %addition
exp(add(E1, E2)) --> ['add'], exp(E1), exp(E2). %subtraction
exp(multiply(E1, E2)) --> ['multiply'], exp(E1), exp(E2). %multiplication
exp(and(E1, E2)) --> ['and'], exp(E1), exp(E2). %and
exp(or(E1, E2)) --> ['or'], exp(E1), exp(E2). %or
exp(not(E)) --> ['not'], exp(E). %not
exp(equal(E1, E2)) --> ['equal'], exp(E1), exp(E2). %equals
exp(greater(E1, E2)) --> ['greater'], exp(E1), exp(E2). %greater
exp(less(E1, E2)) --> ['less'], exp(E1), exp(E2). %less
exp(power(E1, E2)) --> ['power'], exp(E1), exp(E2). % power
exp(var(X, T)) --> [X], { atom(X) }, ['$'], [T]. %variable assignment

% type-checking rules
hastype(_, N, intT) :- number(N).
hastype(_, true, boolT).
hastype(_, false, boolT).
hastype(G, X, T) :- atom(X), lookup_type(X, G, T).
hastype(G, E1 + E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 - E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 * E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 ∧ E2, boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, E1 ∨ E2, boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, ¬E, boolT) :- hastype(G, E, boolT).
hastype(G, E1 = E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 > E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 < E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 ** E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, add(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, subtract(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, multiply(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, and(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, or(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, not(E), boolT) :- hastype(G, E, boolT).
hastype(G, equal(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, greater(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, less(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, power(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, factorial(E), intT) :- hastype(G, E, intT).
hastype(G, var(X, T), T) :- atom(X), assign_type(X, T, G).

% Predicate for assigning types to variables
assign_type(X, T, G) :- \+ member((X, _), G), append([(X, T)], G, NewG), retractall(type_env(_)), assertz(type_env(NewG)).
assign_type(X, T, G) :- member((X, _), G), write('Error: Variable already assigned a type.'), fail.

% % Example test cases
% test_case_var_type :-
%     assign_type(x, intT, []),
%     hastype(['x', intT], var(x, intT), intT),
%     assign_type(y, boolT, [('x', intT)]),
%     hastype(['x', intT, 'y', boolT], var(y, boolT), boolT),
%     write('Variable type assignment and type-checking passed.'), nl.

% % Run the test case
% :- test_case_var_type.
