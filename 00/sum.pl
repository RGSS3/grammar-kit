sum([], 0).
sum([First|Rest], Sum) :-
    sum(Rest, RestSum),
    Sum is First + RestSum.