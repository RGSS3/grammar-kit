Chapter 0 - Preface
=============


## Prolog
We are going to discuss some topics about grammars here, using Prolog. The implementation we use is [swi-prolog](http://www.swi-prolog.org).

For Linux users, like Ubuntu, you can checkout your package manager like
```
sudo apt install swi-prolog
```
Or build from source. Instructions can be found [here](http://www.swi-prolog.org/build/unix.html).

For Windows and Mac users, You can get it here in [http://www.swi-prolog.org/Download.html](http://www.swi-prolog.org/Download.html). 

For the whole book, we assume that executable `swipl` is in environment `PATH`.


This book, however, is not a full Prolog language tutorial. We just provide some basics here. 
 
### Command basic usage
`swipl` can be used as a REPL, or interactive interpreter.

type `swipl` in the command line.
You will see something like this:
```prolog swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 7.6.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

1 ?-
```

the ?- is to prompt you to enter some clause. If you are going to exit, just type `halt.`
Don't forget the last period(.), This just means you finished a group of clauses.

```prolog swipl
1 ?- write('Hello world'), nl.
Hello world
true.

2 ?- A = 3.
A = 3.
```

The two clauses are succeeded, the first just output a string(`write`), then a newline character(`nl`).
The second found a solution(which is identical to the problem), that satisfy all.

For `solution`, let's just look at more examples.

```prolog swipl
3 ?- A = 3, B = 4, C = (A, B), (D, E) = C.
A = D, D = 3,
B = E, E = 4,
C =  (3, 4).
```
This illustrates that Prolog can easily match something with a tuple, or vice versa.

Remeber, order does not matter for clauses that is glued by commas(,).
```prolog swipl
4 ?- A = 3, C = (A, B), C = (3, 10).
A = 3,
C =  (3, 10),
B = 10.
```

In this case, C = (A, B), C is a tuple of A and an uninitialized variable B.  
In C = (3, 10), C is a tuple of 3 and 10.
If we are going to make the expression a truth, we should have A = 3 and B = 10, this does not conflict with other clauses, so this is a solution.

There can be contradictions, and thus Prolog can not find a solution.
```prolog swipl

5 ?- A = 3, A = 5.
false.

6 ?- A = 3, C = (A, B), C = (4, 10).
false.
```

But there is one exception, the last element of a tuple can match the rest of another.
```prolog swipl
7 ?- C = (A, B), C = (4, 10, 7).
C =  (4, 10, 7),
A = 4,
B =  (10, 7).
```

### Prolog file usage
Write a prolog file like:
```prolog 00/hello.pl

hello :-
  write('Hello world'),
  nl.
```

Here `hello` is called 'predicate' in Prolog, even now it looks like just a statement.    
The operator **:-** is used to seperated things, you may read as 
```
`hello` is true if write('Hello world') and nl is also true.
```

Save it as 'hello.pl'.  
```note
Perl source files also have a suffix .pl. Take care of it if you are choosing the file type highlighter in your favourite editor.
```
Then in `swipl` console, we load and test.
```prolog swipl
?- [hello].
true.

?- hello.
Hello world
true.
```

the angle-bracket(`[]`) means to load a file, the `.pl` suffix can be ignored.
However, if you would like to type the suffix as well as some path, it may look like
```prolog swipl
?- ['some path with spaces/to/file.pl'].
```
use single-quoted sign (`'`) to make it a legal atom.


If we would like to write something that tells the maximum of two numbers,
it may look like this:
```prolog 00/max.pl
max(A, B, A) :- A > B.
max(A, B, B) :- A =< B.
```

```
read as if :
  max(A, B, A) will be a truth if A > B.
  max(A, B, B) will be a truth if A <= B.
```

If there is a possibility that more than one solution to get the truth can be found, Prolog will ask you what to do, press semicolon(';') to continue, or press Enter key to stop.

The less-than operator (`=<`) is a little different from other common languages, but others are just the same (`==`, `>`, `<`, `!=`, `<=`)

Test it in your console
```prolog swipl

?- [max].
true.

?- max(3, 5, A).
A = 5.

?- max(10, 1, A).
A = 10 ;
false.

```

The semicolon is just typed by hand, means the first branch matched, there can be more branches matched. After trying another branch, Prolog found no more. So a `false` is printed.


### Lists 1
Prolog also have things like lists.


```prolog swipl
?- A = [1,2,3,4,5,6], [B, C|D] = A.
A = [1, 2, 3, 4, 5, 6],
B = 1,
C = 2,
D = [3, 4, 5, 6]
```
Here B will match the first element of A, C the second,     
and the D after bar(`|`) matches the rest.

If we define the summation of a list recursively like:
#### Definition of sum
 * Empty list have a sum of 0
 * List that can be seen as the `First` element and the `Rest` elements,   
   when the sum of `Rest` is previously calculated as `RestSum`,
   then the sum of the List must be `First + RestSum`

and then using Prlog to express:
```prolog 00/list.pl
sum([], 0).
sum([First|Rest], Sum) :-
    sum(Rest, RestSum),
    Sum is First + RestSum.
```

Try it in Prolog console:
```prolog swipl
?- [sum].
true.

?- sum([], Sum).
Sum = 0.

?- sum([1,2,3,4], Sum).
Sum = 10.

?- sum([-1,1,2,3], Sum).
Sum = 5.
```