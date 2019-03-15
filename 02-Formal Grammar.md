Chapter 2 - Formal Grammar
=============

## 2.1 Formal Grammar
In Chapter 1 you take a glimpse of such so-called grammar. We don't give the formal definition. 
From [wikipage](https://en.wikipedia.org/wiki/Formal_grammar) you will find the formal definition.

Here we only consider what is programmable.


For example, we are considering a language that accept only integer arithmetic strings.
The following strings are in the language.
```
3+5
1
(2+4*5)/2-(3+(4+5))
```
But the following are not:
```
+
()
3-
+5*4
```

We can have this rewriting rule:
```
1. E -> T
2. E -> E '+' T
3. E -> E '-' T
4. T -> F
5. T -> T '*' F
6. T -> T '/' F
7. F -> integer
8. F -> '(' E ')'
```
The `integer` is implicitly defined as an integer (one or more digits(`0-9`)).

For example,  the generation(derivation) of `3+5`:
```
E => E '+' T => T '+' T => F '+' T => 3(integer) '+' T => 3 '+' F => 3 '+' 5
  2          1          4          7                   4          7
```

Here for `E '+' T => T '+' T`
we only use rule 1 to replace the leftmost 'E', keeping the rest untouched.
We always consider the rewriting from the left to the right. This is the `left-most derivation`. There also can be right derivation.

Try it yourself.    
Write the following derivations.
```
3+(5*2)
(5+3+2)*(4/7)
```

All the derivations have something in common, let's summarize.
1.  `Non-terminal`: E, T and F will not appear in the final string, they always derive something.
1.  `Alphabet`: the alphabet we use is just digit(0-9), +, -, *, /, '(', ')'
1.  `Rewriting rule`: We always follow the rewriting rules.
1.  `Start`: Always start derivation from single E.



The formal grammar we just defined is G = {N, Σ, P, S}, where
1. N = {E, T, F}
2. Σ = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, +, -, *, /, (, )}
3. P = {E -> T, E -> E '+' T, ...} 
4. S = E

In general, 
1. N and Σ have no common elements
2. S ∈ N
3. for all rules in P, LHS and RHS are both strings formed from characters in N and Σ.
4. for all rules in P, LHS has at least a character from N.

##  2.2 Decription in Prolog
In Prolog, we can describe the grammar in this way(only for this book, sure you can have other ways).
``` prolog 02/grammar.pl
% grammar.pl
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
```


### Find the LHS and RHS
```prolog swipl
?- [grammar].
true.

?- grammar(G), member(LHS --> _, G).
LHS = 'E' ;
LHS = 'E' ;
LHS = 'E' ;
LHS = 'T' ;
LHS = 'T' ;
LHS = 'T' ;
LHS = 'F' ;
LHS = 'F'.

?- grammar(G), member(_ --> RHS, G).
RHS = ['T'] ;
RHS = ['E', +, 'T'] ;
RHS = ['E', -, 'T'] ;
RHS = ['F'] ;
RHS = ['T', *, 'F'] ;
RHS = ['T', /, 'F'] ;
RHS = [integer] ;
RHS = ['(', 'E', ')'].
```
In the output, the lines of G is filtered for clarity.

Now you can collect them.

```prolog swipl
?- grammar(G), findall(LHS, member(LHS --> _, G), LHSs).
LHSs = ['E', 'E', 'E', 'T', 'T', 'T', 'F', 'F'].

?- grammar(G), findall(RHS, member(_ --> RHS, G), RHSs).
RHSs = [['T'], ['E', +, 'T'], ['E', -, 'T'], ['F'], ['T', *, 'F'], ['T', /, 'F'], [integer], ['('|...]].
```

Now, let's just assume in this book, that LHS's in rewriting rules only contain one Non-terminal. Let's code some helper predicates in Prolog.

```prolog 02/grammar.pl
nonterminals(G, NT) :- 
    % find all LHS
    findall(LHS, member(LHS --> _, G), NT1), 
    % make it a set by removing duplicates
    setof(NT, member(NT, NT1), NT).

terminals(G, T) :- 
    nonterminals(G, NT),
    % find all tokens that appear in the RHS, but not in the LHS
    findall(T1, (member(_ --> RHS, G), 
                member(T, RHS),
              \+member(T, NT)), T1),  % \+ means 'not'
    % make it a set by removing duplicates
    setof(T, member(T, T1), T).
```

Try it
```prolog swipl
?- grammar(G), nonterminals(G, NT).
NT = ['E', 'F', 'T'].

?- grammar(G), terminals(G, T).
T = ['(', ')', *, +, -, /, integer].
```

Here we omitted the detail of `integer`, which actually should be another rule in P.

## 2.3 Prolog' (Derive in Prolog)
First let's rewrite the grammar in this form:
```prolog 02/derive.pl
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
```
So we can refer to any of them by index:
```prolog swipl
?- [derive].
true.

?- g(G), A = G.1.
A =  ('E'-->['T']).
```

Now let's do some exercises:
``` prolog
lmd(Rule, Str, Result) :- fail.
% LMD is short for left-most derivation.
% here we would apply the Rule to the Str, where possible and left-most, the result is Result.
% `fail` means never success, but here it can mean something left to do (TODO).
```

First let's write another helper predicate:
```prolog 02/derive.pl
splitFirst(Str, A, Left, Right) :-
    % append used to glue 2 lists to form the 3rd list
    % we can also use it to split the 3rd list into two.
    append(Left, [A|Right], Str),
    \+member(A, Left).
```

This is going to split Str as three parts: Left, A, Right
```prolog swipl
?- splitFirst([1,2,3,4,5,6,7], 2, L, R).
L = [1],
R = [3, 4, 5, 6, 7] .

?- splitFirst([1,2,3,2,3,2,3,4,5,7],3, L, R).
L = [1, 2],
R = [2, 3, 2, 3, 4, 5, 7] .
```

Then we can do like this:
1. split the string into three part by the non-terminal
2. derive(rewrite) the non-terminal using the rule
3. glue them together.

```prolog 02/derive.pl
lmd(NT --> RHS, Str, Result) :- 
    splitFirst(Str, NT, L, R),
    % append used to glue 2 lists to form the 3rd list
    append(L, RHS, LRHS),
    append(LRHS, R, Result).
```

Try:
```prolog swipl
?- g(G), Str = ['E'], lmd(G.2, Str, A).
G = grammar{1: ('E'-->['T']), 2: ('E'-->['E', +, 'T']), 3: ('E'-->['E', -, 'T']), 4: ('T'-->['F']), 5: ('T'-->['T', *, 'F']), 6: ('T'-->['T', /, 'F']), 7: ('F'-->[integer]), 8: ('F'-->['(', 'E', ')'])},
Str = ['E'],
A = ['E', +, 'T'] .
```

At last, let's apply a sequence of LMD.
```prolog 02/derive.pl
% LMD for sequence
lmds(_, [], Result, Result) :- 
    write(Result), write("."), nl.

lmds(G, [Rule|RestRule], Str, Result) :-
    write(Str), write("-->"), nl,
    lmd(G.Rule, Str, Result1),
    lmds(G, RestRule, Result1, Result).
```


Try it
```prolog swipl
?- g(G), lmds(G, [2, 1, 4, 7, 4, 7], ['E'], R).
[E]-->
[E,+,T]-->
[T,+,T]-->
[F,+,T]-->
[integer,+,T]-->
[integer,+,F]-->
[integer,+,integer].
G = grammar{1: ('E'-->['T']), 2: ('E'-->['E', +, 'T']), 3: ('E'-->['E', -, 'T']), 4: ('T'-->['F']), 5: ('T'-->['T', *, 'F']), 6: ('T'-->['T', /, 'F']), 7: ('F'-->[integer]), 8: ('F'-->['(', 'E', ')'])},
R = [integer, +, integer] .
```

Of course you can make Prolog to guess the sequence.
```prolog 02/derive.pl
% just removed the verbosed output 
lmds1(_, [], Result, Result).

lmds1(G, [Rule|RestRule], Str, Result) :-
    lmd(G.Rule, Str, Result1),
    lmds1(G, RestRule, Result1, Result).
```


Try:
```prolog swipl
26 ?- g(G), length(R, 6), lmds1(G, R, ['E'], [integer, +, integer]).  
R = [2, 1, 4, 4, 7, 7] ;
R = [2, 1, 4, 7, 4, 7] ;
R = [2, 4, 1, 4, 7, 7] ;
R = [2, 4, 1, 7, 4, 7] ;
R = [2, 4, 7, 1, 4, 7] ;
```

See? In the reversed version, we have several ways to approach the answer, but we know only `[2, 1, 4, 7, 4, 7]` is the LMD.
This is because there are branches that like in `E + T`, the `lmds` could replace 'E' or 'T' first, which leads to different version.
If we would like to limit it to generate LMD sequence, we can have a contraint that the non-terminal to rewrite is the first non-terminal.

You will have to modify the predicates:
*  splitFirst to splitFirst2
*  lmd to lmd2
*  lmd1 to lmds2
*  nonterminals from grammar.pl to nt
Here we just leave it as an exercise, only give the expected result:
```prolog swipl
?- [derive].
true.

?- g(G), length(R, 6), lmds2(G, R, ['E'], [integer, +, integer]).
G = grammar{1: ('E'-->['T']), 2: ('E'-->['E', +, 'T']), 3: ('E'-->['E', -, 'T']), 4: ('T'-->['F']), 5: ('T'-->['T', *, 'F']), 6: ('T'-->['T', /, 'F']), 7: ('F'-->[integer]), 8: ('F'-->['(', 'E', ')'])},
R = [2, 1, 4, 7, 4, 7] ;
```


Also, try:
```prolog swipl
8 ?- g(G), Str = [integer, '+', '(', integer, '+', integer, ')'], length(R, N), N = 12, lmds2(G, R, ['E'], Str), lmds(G, R, ['E'], Str).
[E]-->
[E,+,T]-->
[T,+,T]-->
[F,+,T]-->
[integer,+,T]-->
[integer,+,F]-->
[integer,+,(,E,)]-->
[integer,+,(,E,+,T,)]-->
[integer,+,(,T,+,T,)]-->
[integer,+,(,F,+,T,)]-->
[integer,+,(,integer,+,T,)]-->
[integer,+,(,integer,+,F,)]-->
[integer,+,(,integer,+,integer,)].
G = grammar{1: ('E'-->['T']), 2: ('E'-->['E', +, 'T']), 3: ('E'-->['E', -, 'T']), 4: ('T'-->['F']), 5: ('T'-->['T', *, 'F']), 6: ('T'-->['T', /, 'F']), 7: ('F'-->[integer]), 8: ('F'-->['(', 'E', ')'])},
Str = [integer, +, '(', integer, +, integer, ')'],
R = [2, 1, 4, 7, 4, 8, 2, 1, 4|...],
N = 12
```
This is slow, and we had to limit the length to tell whether it's done, since we don't provide any special information about the grammar, and Prolog will try all kinds of possibilities. Later we will disucss different type of grammars and how to deal each of them effectively and efficiently.