g(grammar{
    1: 'E' --> ['T'],
    2: 'E' --> ['E', '+', 'T'],
    3: 'E' --> ['E', '-', 'T'],
    4: 'T' --> ['F'],
    5: 'T' --> ['T', '*', 'F'],
    6: 'T' --> ['T', '/', 'F'],
    7: 'F' --> ['integer'],
    8: 'F' --> ['(', 'E', ')' ]
}).

nt(G, NT) :- 
    findall(LHS, G._ = (LHS --> _), LHS),
    setof(NT, member(NT, LHS), NT).

splitFirst(Str, A, Left, Right) :-
    append(Left, [A|Right], Str),
    \+member(A, Left).


lmd(NT --> RHS, Str, Result) :- 
    splitFirst(Str, NT, L, R),
    append(L, RHS, LRHS),
    append(LRHS, R, Result).

lmds(_, [], Result, Result) :- 
    write(Result), write("."), nl.

lmds(G, [Rule|RestRule], Str, Result) :-
    write(Str), write("-->"), nl,
    lmd(G.Rule, Str, Result1),
    lmds(G, RestRule, Result1, Result).


lmds1(_, [], Result, Result).
lmds1(G, [Rule|RestRule], Str, Result) :-
    lmd(G.Rule, Str, Result1),
    lmds1(G, RestRule, Result1, Result).



splitFirst2(G, Str, A, Left, Right) :-
    append(Left, [A|Right], Str),
    nt(G, NT),
    forall(member(NT1, NT), \+member(NT1, Left)).

lmd2(G, NT --> RHS, Str, Result) :- 
    splitFirst2(G, Str, NT, L, R),
    append(L, RHS, LRHS),
    append(LRHS, R, Result).

lmds2(_, [], Result, Result).
lmds2(G, [Rule|RestRule], Str, Result) :-
    lmd2(G, G.Rule, Str, Result1),
    lmds2(G, RestRule, Result1, Result).
