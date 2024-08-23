% Grammar
exp(N) --> number(N).
exp(true) --> [true].
exp(false) --> [false].
exp(X) --> [X], { atom(X) }.
exp(E1 + E2) --> exp(E1), ['+'], exp(E2).
exp(E1 * E2) --> exp(E1), ['*'], exp(E2).
exp(E1 ∧ E2) --> exp(E1), ['∧'], exp(E2).
exp(E1 ∨ E2) --> exp(E1), ['∨'], exp(E2).
exp(¬E) --> ['¬'], exp(E).
exp(E1 = E2) --> exp(E1), ['='], exp(E2).
exp(E1 > E2) --> exp(E1), ['>'], exp(E2).
exp(E1 < E2) --> exp(E1), ['<'], exp(E2).
exp(E1 ** E2) --> exp(E1), ['**'], exp(E2). % power
exp(factorial(E)) --> exp(E), ['$'].
exp(add(E1, E2)) --> ['add'], exp(E1), exp(E2).
exp(multiply(E1, E2)) --> ['multiply'], exp(E1), exp(E2).
exp(and(E1, E2)) --> ['and'], exp(E1), exp(E2).
exp(or(E1, E2)) --> ['or'], exp(E1), exp(E2).
exp(not(E)) --> ['not'], exp(E).
exp(equal(E1, E2)) --> ['equal'], exp(E1), exp(E2).
exp(greater(E1, E2)) --> ['greater'], exp(E1), exp(E2).
exp(less(E1, E2)) --> ['less'], exp(E1), exp(E2).
exp(power(E1, E2)) --> ['power'], exp(E1), exp(E2).

% Type-checking rules
hastype(_, N, intT) :- number(N).
hastype(_, true, boolT).
hastype(_, false, boolT).
hastype(G, X, T) :- atom(X), lookup_type(X, G, T).
hastype(G, E1 + E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 * E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 ∧ E2, boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, E1 ∨ E2, boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, ¬E, boolT) :- hastype(G, E, boolT).
hastype(G, E1 = E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 > E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 < E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 ** E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E$, boolT) :- hastype(G, E, boolT).
hastype(G, add(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, multiply(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, and(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, or(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, not(E), boolT) :- hastype(G, E, boolT).
hastype(G, equal(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, greater(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, less(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, power(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, factorial(E), intT) :- hastype(G, E, intT).

% Helper predicate for looking up types in the environment
lookup_type(X, [(X, T) | _], T).
lookup_type(X, [_ | Rest], T) :- lookup_type(X, Rest, T).
hastype(G, var(X, T), T) :- atom(X), assign_type(X, T, G).

% Test environment
% Convert environment format [(varName, type)] to the format used in lookup_type/3
convert_env([], []).
convert_env([(Var, Type) | Rest], [(Var, Type) | ConvertedRest]) :-
    convert_env(Rest, ConvertedRest).

% Test environment
test_env([
    ('x', intT),
    ('y', intT),
    ('z', boolT)
]).

% Test case using the environment in the desired format
test_case_7 :-
    test_env(EnvList),
    convert_env(EnvList, Env),
    hastype(Env, x, intT).

% Run the test case
:- test_case_7.
