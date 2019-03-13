is_in_sigma_star1(String, Sigma) :- 
    forall(member(Char, String), member(Char, Sigma)).
 