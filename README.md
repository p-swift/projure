## Projure
This library implements Prolog as an embedded language in Clojure.
The code and documentation are based on the implementation developed by
Peter Norvig in [Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp](https://www.amazon.com/Paradigms-Artificial-Intelligence-Programming-Studies/dp/1558601910/)<sup>[1](#myfootnote1)</sup>.

### Introduction
Prolog is a logic programming language. It has three basic statements:
facts, rules, and queries<sup>[2](#myfootnote2)</sup>. The simplest kind
of statement is a fact. Facts are a way of saying that there is a relationship
between two objects. Here are some facts:
```
(likes Kim, Robin)
(likes Robin, cats)
```
These facts say that Kim likes Robin and Robin likes cats. In order to assert 
facts in Clojure, we will need some new syntax so it knows that `likes` is a 
fact and not a function call. Traditionally that is the `<-` macro. And once 
we define that macro we can properly add facts to the database like this:
```
(<- (likes Kim Robin))
(<- (likes Robin cats))
```
The second kind of statement in a logic program is a query. 
Queries are a way of retrieving information from a Prolog program.
A query asks whether a certain relation (like the
relationship between Kim and Robin) holds between objects. Prolog
has an interactive programming environment, much the like read-eval-print loop
in Lisp. We will define the `?-` macro for interactive queries, and it will work
something like this:
```
> (?- (likes Kim ?who))
Robin
> (?- (likes ?who Robin))
Kim
```
In the first case, the variable  `?who` is bound to Robin, while in the
second, `?who` bound to Kim. In Prolog, logic variables can be bound in any position.
We need only a single rule to ask "who does Kim like" and "who likes Kim".
In Clojure, however, we would define a function `likes`, so that (likes 'Kim) would return the list (Lee Kim).
If we wanted to access the information the other way, we would need to define 
another function, say, `likers-of`, so that `(likers-of 'Lee)`
returns `(Sandy)`. 

This brings us to the *rule*, which the most important statement in logic programming.
Rules allow us to define new relationships in terms of existing
relationships. They can be thought of as *virtual* facts because they imply facts
which are not directly stored in a database.
```
(<- (likes Sandy ?x) (likes ?x cats))
```
"This can be read in two ways," Norvig writes. "Viewed as a logical 
assertion, it is read, "For any x, Sandy likes x if x likes cats."
This is a declarative interpretation. Viewed as a piece of a Prolog program, 
it is read, "If you ever want to show that Sandy likes some x, one way 
to do it is to show that x likes cats." This is a procedural interpretation. 
It is called a backward-chaining interpretation, because one reasons backward 
from the goal (Sandy likes x) to the premises (x likes cats)." 
   
A clause asserts that the head is true if and only if all the goals in the body are true. For example, the following clause says that Kim likes anyone who likes both Lee and Kim:
```
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
```   
Another way to interpret this is to say "For any x, deduce that 
Kim likes x if it can be proved that x likes Lee and x likes Kim."
### User Interface
Some commonly used Prolog predicates are already defined. More may be added in the future.
 ```
 =/2 ==/2 and/2 append/3 bagof/3 call/1 member/2 or/2 setof/3
```

#### Defined Clojure Operators
| Clojure Operator         | Description                                                                                                                                                                                                        |
| -------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `<-`                     | Assert a fact or rule (ie add a clause to the database). Examples:<br>```(<- (likes Robin cats))```<br>```(<- (likes Sandy ?x) (likes ?x cats))```<br>                                                             |
| `<--`                    | Assert a fact or rule, but first retract all facts and rules for the functor with the same arity.<br>```(<-- (member ?item (?item . ?rest)))```<br>```(<- (member ?item (?x . ?rest)) (member ?item ?rest))```<br> |                                                                                                                                  
| `fact`                   | Assert a fact `(fact (likes Robin cats))`. Same as `<-`.                                                                                                                                                           |
| `rule`                   | Assert a rule . Same as `<-` but with an `if` separating each clause.<br> `(rule (likes Sandy ?x) if (likes ?x cats))`                                                                                             |
| `?-`                     | Interactively try to prove the concatenation of clauses, printing unified variables and then backtracking after each success.                                                                                      |

| Prolog Functor           | Description                                                                                                                                                 |
| -------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `lisp`                   | Execute a Lisp form from Prolog (example below).                                                                                                            |
| `lispp`                  | Convenience predicate which fails if execution of the form returns false. Example:<br/> ```(<-- (positives ?x ?list) (member ?x ?list) (lispp (> ?x 0)))``` |

#### The Interface between Prolog and Clojure

| Clojure Operator         | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| -------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `prolog`                 | Invoke Prolog from Clojure to solve a conjunction of clauses. The Clojure environment can be accessed through the `lisp` functor. Here `?person` is unified with value of `person` from the surrounding lexical environment. <br>```(defn friends-of [person]```<br>&nbsp;&nbsp;```(let [friends (transient[])]```<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;```(prolog (lisp ?person person)```<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;```(likes ?person ?x)```<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;```(lisp (conj! friends ?x)))```<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;```(persistent! friends)))```<br> |
| `query`                  | Convenience operator which invokes Prolog in the surrounding Clojure environment, and returns a vector containing each success. Shorthand for above example.<br>```(defn friends-of [person]```<br>&nbsp;&nbsp;```(query ?x (lisp ?person person)```<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;```(likes ?person ?x)))```<br>                                                                                                                                                                                                                                                                                                                                                       |       

### Examples
__member/2__\
Like Lisp programmers Prolog programmers often work with lists and recursion.
But what looks like an algorithm is Lisp often looks like a simple restatement
of the facts in Prolog. For example, if we define `member` like this:
* an item is a member of a list if it is the first element of the list;
* it can also be a member if it is the first element of the rest of the list;

it can be translated directly into Prolog.
```clojure
(<-- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))
```
The 'dot' means 'cons' in others lisps, and it here destructures the parameter list into 'first' and 'rest'.
To find all solutions to `member` (ie every item), we can use the `query` operator which is defined above.
```clojure
> (query ?x (member ?x [1 2 3]))
[1 2 3]
```

__quicksort__
```
(<-- (append nil ?ys ?ys))
(<- (append (?x . ?xs) ?ys (?x . ?zs)) (append ?xs ?ys ?zs))

(<-- (quicksort (?x . ?xs) ?ys)
     (partition ?xs ?x ?littles ?bigs)
     (quicksort ?littles ?ls)
     (quicksort ?bigs ?bs)
     (append ?ls (?x . ?bs) ?ys))
(<- (quicksort nil nil))

(<-- (partition (?x . ?xs) ?y (?x . ?ls) ?bs)
     (lispp (<= ?x ?y))
     (partition ?xs ?y ?ls ?bs))
(<- (partition (?x . ?xs) ?y ?ls (?x . ?bs))
    (lispp (> ?x ?y))
    (partition ?xs ?y ?ls ?bs))
(<- (partition nil ?y nil nil))
```

```
> (?- (quicksort [3 2 1] ?x))
[1 2 3]
```
### Implementation
(Sorry this is just a skeleton for now; please refer to the actual code or to [PAIP](https://github.com/norvig/paip-lisp) chapter 12 for details.)

Following Norvig's convention (and example), we will compile each Prolog predicate 
into a single Clojure function.
```
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
```
But what should each function look like? Let's take the `likes` predicate:
```clojure
(defn likes-2 [?arg1 ?arg2 trail cont]
  (let [old-trail (size trail)]
    ;; first clause
    (if (unify! ?arg1 'Robin trail)
      (if (unify! ?arg2 'cats trail)
        (cont)))
    (undo-bindings! trail old-trail)
    ;; second clause
    (if (unify! ?arg1 'Sandy trail)
      (likes-2 ?arg2 'cats trail cont))
    (undo-bindings! trail old-trail)
    ;; third clause
    (if (unify! ?arg1 'Kim trail)
      (likes-2 ?arg2 'Lee trail
               (fn [] (likes-2 ?arg2 'Kim trail cont))))))
```
And for Prolog predicate `member`
```
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))
```
we generate the following code
```clojure
(defn member-2 [?arg1 ?arg2 trail cont]
  (let [old-trail (size trail)]
    ;; first clause
    (if (unify! ?arg2 (pair ?arg1 (?)) trail)
      (cont))
    (undo-bindings! trail old-trail)
    ;; second clause
    (let [?rest (?)]
      (if (unify! ?arg2 (pair (?) ?rest) trail)
        (member-2 ?arg1 ?rest trail cont)))))
```
One implementation detail worth noticing here is that we are create a new type of
object with `pair`
```clojure
;; Pair is like a list where the last element might not be nil.
;; In other lisps, it is called 'cons' and it is used to build
;; lists which can proper (if they end with nil) or improper/dotted
;; if they don't. The reason we need Pair is that Prolog predicates
;; like member/2 and append/3 often recurse down a list by destructing
;; the parameter list, and then unifying the first and the rest of it.
(deftype Pair [first rest])
```
### Benchmarking the Compiler 
The zebra puzzle is an unofficial benchmark for testing the speed of
a Prolog implementation. The puzzle consists of five different-colored houses in a row,
each lived in by a resident of a different nationality. Each resident owns a 
different pet, prefers a different drink, and smokes a different brand of 
cigarettes than the others. There are fifteen facts:
```
1. There are five houses.
2. The Englishman lives in the red house. 
3. The Spaniard owns the dog.
4. Coffee is drunk in the green house. 
5. The Ukrainian drinks tea.
6. The green house is immediately to the right of the ivory house.
7. The Winston smoker owns snails.
8. Kools are smoked in the yellow house.
9. Milk is drunk in the middle house.
10. The Norwegian lives in the first house on the left.
11. The man who smokes Chesterfields lives next to the man with the fox.
12. Kools are smoked in the house next to the house with the horse.
13. The Lucky Strike smoker drinks orange juice.
14. The Japanese smokes Parliaments.
15. The Norwegian lives next to the blue house.
```
The goal is a find out "who drinks water?" and "who owns the zebra?" (hence the
name "zebra" puzzle). Prolog is very good at logic puzzles because it can
generate and test solutions. Pairing unification with depth-first search
is particularly effective because unification whittles down the search space 
while automatic backtracking frees the programmer from stating explicitly
how the search will be carried out. Remember, we want to answer the following
questions:
```
1. Who drinks water?
2. Who owns the zebra?
``` 
The zebra relationship relies on a couple of auxiliary  relationships
which are defined first. The relationship `nextto` (for "next to") 
and `iright` (for "immediately to the right of") are similar to `member`, which is repeated here.
```
(<-- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<-- (nextto ?x ?y ?list) (iright ?x ?y ?list))
(<-  (nextto ?x ?y ?list) (iright ?y ?x ?list))

(<-- (iright ?left ?right (?left ?right . ?rest)))
(<-  (iright ?left ?right (?x . ?rest)) (iright ?left ?right ?rest))

;; Equality predicate which succeeds if two varibles
;; are "equal" in the sense that they can be unified.
(<- (= ?x ?x))
```
And with that we are ready to define the zebra puzzle with a single clause.
Remember, each house is of the form:
(house nationality pet cigarette drink house-color)
```
(<-- (zebra ?h ?w ?z)
     (= ?h ((house norwegian ? ? ? ?)               ;1,10
            ?
            (house ? ? ? milk ?) ? ?))              ; 9
     (member (house englishman ? ? ? red) ?h)       ; 2
     (member (house spaniard dog ? ? ?) ?h)         ; 3
     (member (house ? ? ? coffee green) ?h)         ; 4
     (member (house ukrainian ? ? tea ?) ?h)        ; 5
     (iright (house ? ? ? ? ivory)                  ; 6
             (house ? ? ? ? green) ?h)
     (member (house ? snails winston ? ?) ?h)       ; 7
     (member (house ? ? kools ? yellow) ?h)         ; 8
     (nextto (house ? ? chesterfield ? ?)           ;11
             (house ? fox ? ? ?) ?h)
     (nextto (house ? ? kools ? ?)                  ;12
             (house ? horse ? ? ?) ?h)
     (member (house ? ? luckystrike oj ?) ?h)       ;13
     (member (house japanese ? parliaments ? ?) ?h) ;14
     (nextto (house norwegian ? ? ? ?)              ;15
             (house ? ? ? ? blue) ?h)
     (member (house ?w ? ? water ?) ?h)             ;Q1
     (member (house ?z zebra ? ? ?) ?h))            ;Q2
```

### Real Prolog
This is what the zebra puzzle looks like in *real* Prolog: 
variables begin with an upper-case letter or underscore; anonymous variables 
are represented by a single underscore; flag and repeat allow sequencing; and  "!" is the cut operator.
```
nextto(X, Y, List) :- iright(X, Y, List).
nextto(X, Y, List) :- iright(Y, X, List).

iright(Left, Right, [Left, Right | _]).
iright(Left, Right, [_ | Rest]) :- iright(Left, Right, Rest).

zebra(H, W, Z) :-
    H = [house(norwegian, _, _, _, _), _, house(_, _, _, milk, _), _, _],
     member(house(englishman, _, _, _, red), H),
     member(house(spaniard, dog, _, _, _), H),
     member(house(_, _, _, coffee, green), H),
     member(house(ukrainian, _,  _, tea, _), H),
     iright(house(_, _, _, _, ivory), house(_, _, _, _, green), H),
     member(house(_, snails, winston, _, _), H),
     member(house(_, _, kools, _, yellow), H),
     nextto(house(_, _, chesterfield, _, _), house(_, fox, _, _, _), H),
     nextto(house(_, _, kools, _, _), house(_, horse, _, _, _), H),
     member(house(_, _, luckystrike, oj, _), H),
     member(house(japanese, _, parliaments, _, _), H),
     nextto(house(norwegian, _, _, _, _), house(_, _, _, _, blue), H),
     member(house(W, _, _, water, _), H),
     member(house(Z, zebra, _, _, _), H).

zebra1(Houses, WaterDrinker, ZebraOwner) :-
        zebra(Houses, WaterDrinker, ZebraOwner).

benchmark1 :-
        flag(benchmark,_,0),
        repeat,
        zebra1(Houses, WaterDrinker, ZebraOwner),
        flag(benchmark,N,N+1),
        N = 1000,
        !.

benchmark :- time(benchmark1).
```

#### The Cut Operator
*todo: implement the cut operator*

#### But is it really a _compiler_? And what about warren abstract machine?<sup>[4](#myfootnote4)</sup>

### References
<a name="myfootnote1">1</a>[Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp](https://github.com/norvig/paip-lisp)\
<a name="myfootnote1">2</a>[The Art of Prolog: Advanced Programming Techniques](https://www.amazon.com/Art-Prolog-Second-Programming-Techniques/dp/0262193388)\
<a name="myfootnote1">3</a>[On Lisp: Advanced Techniques for Common Lisp](http://www.paulgraham.com/onlisp.html)\
<a name="myfootnote1">4</a>[Warren's Abstract Machine: A Tutorial Reconstruction](https://www.amazon.com/Warrens-Abstract-Machine-Reconstruction-Programming/dp/0262510588)

### License

MIT License

Copyright (c) 2020 p-swift

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
