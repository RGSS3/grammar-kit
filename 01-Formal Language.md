Chapter 1 - Formal Language
=============

## 1.1 Formal Language

From the wikipedia
```
A formal language L over an alphabet Σ is a subset of Σ*, that is, a set of words over that alphabet. Sometimes the sets of words are grouped into expressions, whereas rules and constraints may be formulated for the creation of 'well-formed expressions'.
```



## 1.2 Alphabet
We can use a list to represent an alphabet
```prolog 01/alphabet.pl
% alphabet.pl
alphabet([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, +, =]).
```

if you try this in console:
```prolog swipl
?- [alphabet].
true.

?- alphabet(A), [First|Rest] = A.
A = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
First = 0,
Rest = [1, 2, 3, 4, 5, 6, 7, 8, 9|...].
```

Use `member/2` to check if a character is in the alphabet.  
   * `member/2` is predefined in the library  
   * `/2` means it has two arguments.

```prolog swipl
?- alphabet(A), member(0, A).
A = [0, 1, 2, 3, 4, 5, 6, 7, 8|...] .

?- alphabet(A), member(a, A).
false.
```


`member/2` can be used to enumerate a list, too:
```prolog swipl
?- alphabet(A), member(B, A).
A = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
B = 0 ;
A = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
B = 1 ;
A = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
B = 2 ;
A = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
B = 3 ;
A = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
B = 4
```
Keep press semicolon(;) and it will print the whole possibility of B.


## 1.3 Σ and Σ*
The variable 'B' in the last section traverses from the alphabet Σ, 
then what is Σ*?

normally, <i>a<sup>n</sup></i>means 'a' repeated n times.   
The star(`*`) means zero or more times, (just like in the Regular Expressions).     
Any string ∈ Σ* iff the string has one of the following properties:    
### definition 1
   1. is empty
   1. contains only characters that belongs to Σ      


When the second line gets rewrited using the `First|Rest` pattern,
### definition 2
   1. is empty
   1. First ∈ Σ, Rest ∈ Σ*

Sometimes we call the empty string `ε`(epsilon)

For `definition 2`, we can have the Prolog definition:
```prolog 01/def2.pl
% def2.pl
is_in_sigma_star2([], _).
is_in_sigma_star2([First|Rest], Sigma) :-
    member(First, Sigma),
    is_in_sigma_star2(Rest, Sigma).
```

The underscore(`_`) is a variable that its value and bindings is not concerned.

For `definition 1`, we can have another Prolog definition:
```prolog 01/def2.pl
% def1.pl
is_in_sigma_star1(String, Sigma) :- 
   forall(member(Char, String), member(Char, Sigma)).
```


Here we use a list to represent a string:
```
A = [h, e, l, l, o, ' ', w, o, r, l, d]. % used to represent "hello world"
```

Try
```prolog swipl
?- alphabet(S), is_in_sigma_star2([], S).
S = [0, 1, 2, 3, 4, 5, 6, 7, 8|...].

?- alphabet(S), is_in_sigma_star2([1, 2, +, 5], S).
S = [0, 1, 2, 3, 4, 5, 6, 7, 8|...] .

?- alphabet(S), is_in_sigma_star2([a, 1, 2, +, 5], S).
false.
```

The second goal fails because 'a' is not in this alphabet.
Replace `is_in_sigma_star2` with `is_in_sigma_star1` will get the same result.

But there is a difference between the two implemenetations:   
As is in the previous section, `member/2` can be used to generate possibilities.
Try
```
?- [alphabet].
true.

?- [def2].
true.

?- alphabet(S), is_in_sigma_star2(A, S).
S = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
A = [] ;
S = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
A = [0] ;
S = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
A = [0, 0] ;
S = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
A = [0, 0, 0] ;
S = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
A = [0, 0, 0, 0] ;
S = [0, 1, 2, 3, 4, 5, 6, 7, 8|...],
A = [0, 0, 0, 0, 0] .
```
This will yield infinite number of possibilities, press period(.) or Enter key to stop this generation process.

If we are going to generate all possible strings that belong to Σ* and have at most N characters. We will have another definition:

### Definition for sigma_string(Str, N, Sigma) 
sigma_string(Str, N, Sigma) succeeds iff Str belongs to Σ* and has most N characters. or any of the following is true
   1. Empty string belongs to Σ*, no matter what N is (of course N >= 0)     
      i.e. sigma_string(Str, _, Sigma) where Str = []
   1. Break String as First and Rest,     
      First must belong to Σ,    
      And Rest must have no more than N - 1 characters and belong to Σ*    
      i.e. sigma_string(Rest, M, Sigma)    
         where M is N - 1,

All of this requires N is a natural number or N >= 0.

in Prolog language, that is:
```prolog 01/def3.pl
% def3.pl
sigma_string([], N, _) :- N >= 0.
sigma_string([First|Rest], N, Sigma) :-
    N >= 0,
    member(First, Sigma),
    M is N - 1,
    sigma_string(Rest, M, Sigma).
```
Notice that you can't just write sigma_string(Rest, N - 1, Sigma) in the last Line.
Because Prolog will not automatically calculate N - 1 as a expression, rather a construction that we will later explain. It's the operator `is` who did the calculation.

Try:   
at this time we only use a small alphabet like {a, b},
```prolog swipl
?- [def3].
true.

?- sigma_string(A, 3, [a, b]).
A = [] ;
A = [a] ;
A = [a, a] ;
A = [a, a, a] ;
A = [a, a, b] ;
A = [a, b] ;
A = [a, b, a] ;
A = [a, b, b] ;
A = [b] ;
A = [b, a] ;
A = [b, a, a] ;
A = [b, a, b] ;
A = [b, b] ;
A = [b, b, a] ;
A = [b, b, b] ;
false.
```

## 1.4 Grammar
Now we know what's Σ*, but a string that belongs to Σ* may not belong to the language L. Some languages requires some contraints.

for example:   
L = {a<sup>n</sup>b<sup>n</sup>|n >= 0}    
L has an alphabet of {a, b}, 

but requires the string formed with a certain number of character 'a' then the same number of character 'b'.

e.g.   
```
ε, ab, aabb, aaabbb ∈ Σ* , also ∈ L   
aab, abb, a, b, baa ∈ Σ* , but not ∈ L    
```

we may use a `grammar` to describe such language:
```
  1. S => ε
  2. S => aSb
```

S => aSb can be read as "S can be rewrited as aSb if possible"
or "S generates string aSb"

to prove `aabb` is a string in L, the following derivation(rewriting) can be taken:

```
S => aSb => aaSbb => aaεbb
  2      2        1    
```
Since the ε denotes the empty string, the result is aabb.

In swi-prolog, you can do the generation work with an extension, which will probably be in the ISO prolog in the future.

The rewrite rule is   
   * rewrite S in lowercase
   * rewrite ε in []
   * rewrite single character in list, e.g. `a` is to be `[a]`

```prolog 01/genL.pl
% genereator for L
s --> [].             % S => ε
s --> [a], s, [b].    % S => a S b
```

```prolog swipl
?- [genL].
true.

?- phrase(s, A).
A = [] ;
A = [a, b] ;
A = [a, a, b, b] ;
A = [a, a, a, b, b, b] ;
A = [a, a, a, a, b, b, b, b] ;
A = [a, a, a, a, a, b, b, b, b|...] .
```

More information and notation can be found on [this wiki page](https://en.wikipedia.org/wiki/Formal_grammar#Example), here we only care about the form of expression. And the definition of `S` is not given. All things left here will be discussed in detail in the next chapter.


