sigma_string([], N, _) :- N >= 0.
sigma_string([First|Rest], N, Sigma) :-
    N >= 0,
    member(First, Sigma),
    M is N - 1,
    sigma_string(Rest, M, Sigma).