grammar([
    'E' --> ['T'],
    'E' --> ['E', '+', 'T'],
    'E' --> ['E', '-', 'T'],
    'T' --> ['F'],
    'T' --> ['T', '*', 'F'],
    'T' --> ['T', '/', 'F'],
    'F' --> ['integer'],
    'F' --> ['(', 'E', ')' ]
]).

nonterminals(G, NT) :- 
    findall(LHS, member(LHS --> _, G), NT1), 
    setof(NT, member(NT, NT1), NT).

terminals(G, T) :- 
    nonterminals(G, NT),
    findall(T1, (member(_ --> RHS, G), 
                 member(T1, RHS),
                 \+member(T1, NT)), T1),
    setof(T, member(T, T1), T).



