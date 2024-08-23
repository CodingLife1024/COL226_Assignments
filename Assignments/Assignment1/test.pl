:- use_module(library(lists)).

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


% Base case: Empty list is reflexive-symmetric-transitive closure.
rstc([], _).

% Reflexive case: (x, x) should be in s.
rstc([[X, X] | Rest], S) :-
    (member([_, X], S); member([X, _], S)),
    rtc(Rest, S).

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