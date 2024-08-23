:- use_module(library(lists)).

% Reflexive-Transitive Closure
% Base case: Empty list is reflexive-transitive closure.
rtc([], _).

% Reflexive case: (x, x) should be in s.
rtc([[X, X] | Rest], S) :-
    (member([_, X], S); member([X, _], S)),
    rtc(Rest, S).

% Transitive case: (x, y) should be in s, and (y, z) should be in rtc.
rtc([[X, Y] | Rest], S) :-
    member([X, Y], S),
    rtc_transitive([X, Y], S),
    rtc([[Y, Z] | Rest], S).

% Helper predicate to check transitivity.
rtc_transitive([X, Y], S) :-
    (member([X, Z], S), member([Z, Y], S));
    rtc_transitive([X, Z], S), rtc_transitive([Z, Y], S).

% The main predicate to check if r is a reflexive-transitive closure over s.
is_rtc_closure([], _).
is_rtc_closure([X, Y], S) :-
    rtc([[X, Y]], S);
    rtc_transitive([X, Y], S).
is_rtc_closure([X, Y | Rest], S) :-
    rtc([[X, Y]], S);
    rtc_transitive([X, Y], S),
    is_rtc_closure([Y | Rest], S).


% Reflexive-Symmetric-Transitive Closure
% Base case: Empty list is reflexive-symmetric-transitive closure.
rstc([], _).

% Reflexive case: (x, x) should be in s.
rstc([[X, X] | Rest], S) :-
    (member([_, X], S); member([X, _], S)),
    rstc(Rest, S).

% Symmetric case
rstc([[X, Y] | Rest], S) :- 
    (member([X, Y], S); member([Y, X], S));
    (member([X, Z], S), member([Z, Y], S));
    (member([Z, X], S), member([Z, Y], S));
    (member([X, Z], S), member([Y, Z], S));
    (member([Z, X], S), member([Y, Z], S)),
    rstc(Rest, S).

% Transitive case: (x, y) should be in s, and (y, z) should be in rtc.
rstc([[X, Y] | Rest], S) :-
    member([X, Y], S),
    rstc_transitive([X, Y], S),
    rstc([[Y, Z] | Rest], S).

% Helper predicate to check transitivity.
rstc_transitive([X, Y], S) :-
    (member([X, Z], S), member([Z, Y], S));
    (member([Z, X], S), member([Z, Y], S));
    (member([X, Z], S), member([Y, Z], S));
    (member([Z, X], S), member([Y, Z], S)),
    rstc_transitive([X, Z], S), rstc_transitive([Z, Y], S).

% The main predicate to check if r is a reflexive-symmetric-transitive closure over s.
is_rstc_closure([], _).
is_rstc_closure([X, Y], S) :-
    rstc([[X, Y]], S);
    rstc_transitive([X, Y], S).
is_rstc_closure([X, Y | Rest], S) :-
    rstc([[X, Y]], S);
    rstc_transitive([X, Y], S),
    is_rstc_closure([Y | Rest], S).

% _____________


del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.

remdups([ ], [ ]) :- !.
remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).

unionI([ ], S2, S2) :- !.
unionI(S1, [ ], S1) :- !.
unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).

append( [ ], L, L).
append( [X|R], L, [X|Z]) :- append(R, L, Z).

mapcons(_, [], []).
mapcons(X, [Y|R], [Z|W]) :- append([X], Y, Z), mapcons(X, R, W).

% powerI([1, 2, 3, 4], Result).
powerI([ ], [ [ ] ]) :- !.
powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).

member(X, [X|_]) :- !.
member(X, [_|T]) :- member(X, T).

% interI([1, 2, 3], [3, 4, 5], Result).
interI([], _, []).
interI([X|R], S2, [X|Z]) :- member(X, S2), interI(R, S2, Z).
interI([_|R], S2, Z) :- interI(R, S2, Z).

% diffI([1, 2, 3], [2, 3, 4], Result).
diffI([], _, []).
diffI([X|R], S2, Z) :- member(X, S2), diffI(R, S2, Z).
diffI([X|R], S2, [X|Z]) :- \+ member(X, S2), diffI(R, S2, Z).

% cartesianI([1, 2, 3], [4, 5], Result).
cartesianI([], _, []).
cartesianI([X|R], S2, Z) :- cartesianI(R, S2, Rest), mapcons(X, S2, Pairs), append(Pairs, Rest, Z).

% test part

% outputs true if 2 sets are same.
sets_same([], []).
sets_same(Set1, Set2) :-
    sort(Set1, SortedSet1),
    sort(Set2, SortedSet2),
    SortedSet1 = SortedSet2.

% outputs true if set of sets is same.
setsOfSets_same([], []).
% Predicate to check if sets are the same
setsOfSets_same([Set1 | Rest1], [Set2 | Rest2]) :-
    sort(Set1, SortedSet1),
    sort(Set2, SortedSet2),
    SortedSet1 = SortedSet2,
    setsOfSets_same(Rest1, Rest2).

is_empty([]).
remove_empty_lists(ListOfLists, Result) :-
    exclude(is_empty, ListOfLists, Result).

lists_same(List1, List2) :-
    sort(List1, SortedList1),
    sort(List2, SortedList2),
    SortedList1 = SortedList2.

check_union :-
    (   unionI([1, 2, 3, 4], [3, 3, 4, 5], Result1),
        sets_same(Result1, [1, 2, 3, 4, 5]),
        writeln('Union Test 1 passed')
    ;   writeln('Union Test 1 failed'), writeln(Result1)
    ),

    (   unionI([1, 2, 3], [3, 3, 4, 5], Result2),
        sets_same(Result2, [1, 2, 3, 4, 5]),
        writeln('Union Test 2 passed')
    ;   writeln('Union Test 2 failed'), writeln(Result2)
    ),

    (   unionI([1, 2, 3, 4, 5], [3, 3, 4, 5], Result3),
        sets_same(Result3, [1, 2, 3, 4, 5]),
        writeln('Union Test 3 passed')
    ;   writeln('Union Test 3 failed'), writeln(Result3)
    ).

check_inter :-
    (   interI([1, 2, 3, 4], [3, 3, 4, 5], Result1),
        sets_same(Result1, [3, 4]),
        writeln('Intersection Test 1 passed')
    ;   writeln('Intersection Test 1 failed'), writeln(Result1)
    ),

    (   interI([1, 2, 3], [3, 3, 4, 5], Result2),
        sets_same(Result2, [3]),
        writeln('INtersection Test 2 passed')
    ;   writeln('Intersection Test 2 failed'), writeln(Result2)
    ),

    (   interI([1, 2, 3, 4, 5], [3, 3, 4, 5], Result3),
        sets_same(Result3, [3, 4, 5]),
        writeln('INtersection Test 3 passed')
    ;   writeln('INtersection Test 3 failed'), writeln(Result3)
    ).

check_diff :-
    (   diffI([1, 2, 3, 4], [3, 3, 4, 5], Result1),
        sets_same(Result1, [1, 2]),
        writeln('Difference Test 1 passed')
    ;   writeln('Difference Test 1 failed'), writeln(Result1)
    ),

    (   diffI([1, 2, 3], [3, 3, 4, 5], Result2),
        sets_same(Result2, [1, 2]),
        writeln('Difference Test 2 passed')
    ;   writeln('Difference Test 2 failed'), writeln(Result2)
    ),

    (   diffI([1, 2, 3, 4, 5], [3, 3, 4, 5], Result3),
        sets_same(Result3, [1, 2]),
        writeln('Difference Test 3 passed')
    ;   writeln('Difference Test 3 failed'), writeln(Result3)
    ).

check_power :-
    (   powerI([1, 2, 3], Result1),
        remove_empty_lists(Result1, Res1),
        setsOfSets_same(Res1, [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3]]),
        writeln('Power Test 1 passed');
        writeln('Power Test 1 failed'), writeln(Result1)
    ),

    (   powerI([1, 2, 3, 4], Result2),
        remove_empty_lists(Result2, Res2),
        setsOfSets_same(Res2, [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4]]),
        writeln('Power Test 2 passed');
        writeln('Power Test 2 failed'), writeln(Result2)
    ),

    (   powerI([1, 2], Result3),
        remove_empty_lists(Result3, Res3),
        setsOfSets_same(Res3, [[1,2],[1],[2]]),
        writeln('Power Test 3 passed');
        writeln('Power Test 3 failed'), writeln(Result3)
    ).

check_cartesian :-
    (   cartesianI([1, 2, 3], [4, 5], Result1),
        lists_same(Result1, [[1|4],[1|5],[2|4],[2|5],[3|4],[3|5]]),
        writeln('Cartesian Test 1 passed');
        writeln('Cartesian Test 1 failed')
    ),

    (   cartesianI([1, 2, 3], [4], Result2),
        lists_same(Result2, [[1|4],[2|4],[3|4]]),
        writeln('Cartesian Test 2 passed');
        writeln('Cartesian Test 2 failed')
    ),

    (   cartesianI([1, 2, 3], [4, 5, 6], Result3),
        lists_same(Result3, [[1|4],[1|5],[1|6],[2|4],[2|5],[2|6],[3|4],[3|5],[3|6]]),
        writeln('Cartesian Test 3 passed');
        writeln('Cartesian Test 3 failed')
    ).
