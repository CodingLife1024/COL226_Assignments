/*  del(X,L1,L2) -- delete element X from a list L1 to obtain L2 */
del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.

/*  remdups(L, L1) remove duplicates from a list L to get L1 */
remdups([ ], [ ]) :- !.
remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).

/* Assuming no duplicates in S1, S2 here is an implementation of union of S1, S2 */
unionI([ ], S2, S2) :- !.
unionI(S1, [ ], S1) :- !.
unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).

/* append(L1, L2, L3) -- append list L1 to list L2 to get list  L3 */
append( [ ], L, L). 
append( [X|R], L, [X|Z]) :- append(R, L, Z).

/* mapcons(X,L1, L2) --  cons the element X to each list in L1 to get L2 */

mapcons(_, [], []).
mapcons(X, [Y|R], [Z|W]) :- append([X], Y, Z), mapcons(X, R, W).

/* powerI( S, P1): Here is an implementation of powerset of S */
powerI([ ], [ [ ] ]) :- !.
powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).

% 1. Check with sufficient examples  that unionI and powerI indeed implement union and power.
% 2. Check that union does not have duplicates.
% 3. Assuming no duplicates in lists representing S1 and S2, write a PROLOG program  interI(S1, S2, S3) that implements intersection of two finite sets.
% 4. Assuming no duplicates in lists representing S1 and S2, write a PROLOG program  diffI(S1, S2, S3) that implements set-difference of two finite sets.
% 5. Assuming no duplicates in lists representing S1 and S2, write a PROLOG program  cartesianI(S1, S2, S3) that implements cartesian of two finite sets.
% 6. Provide sufficient test cases examples to demonstrate your implementations are correct.
% 7. Suggest a way to check that the powersets obtained from the implementation of two different valid representations of a set (elements given in different order) are equal.

% member(X, L): Check if X is a member of list L
member(X, [X|_]) :- !.
member(X, [_|T]) :- member(X, T).

% interI(S1, S2, S3): Implement the intersection of sets S1 and S2 to get S3
interI([], _, []).
interI([X|R], S2, [X|Z]) :- member(X, S2), interI(R, S2, Z).
interI([_|R], S2, Z) :- interI(R, S2, Z).

% diffI(S1, S2, S3): Implement the set difference of sets S1 and S2 to get S3
diffI([], _, []).
diffI([X|R], S2, Z) :- member(X, S2), diffI(R, S2, Z).
diffI([X|R], S2, [X|Z]) :- \+ member(X, S2), diffI(R, S2, Z).

% cartesianI(S1, S2, S3): Implement the cartesian product of sets S1 and S2 to get S3
cartesianI([], _, []).
cartesianI([X|R], S2, Z) :- cartesianI(R, S2, Rest), mapcons(X, S2, Pairs), append(Pairs, Rest, Z).