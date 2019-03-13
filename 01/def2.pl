is_in_sigma_star2([], _).
is_in_sigma_star2([First|Rest], Sigma) :-
    member(First, Sigma),
    is_in_sigma_star2(Rest, Sigma).
