/* Results with SWI Prolog
 *
 * with cut
 * ?- benchmark.
 * % 12,852,846 inferences, 4.647 CPU in 4.674 seconds (99% CPU, 2765828 Lips)
 *
 * without cut
 * ?- benchmark.
 * % 29,289,845 inferences, 10.596 CPU in 10.756 seconds (99% CPU, 2764170 Lips)
 */
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
        zebra(Houses, WaterDrinker, ZebraOwner), !.

benchmark1 :-
        flag(benchmark,_,0),
        repeat,
        zebra1(Houses, WaterDrinker, ZebraOwner),
        flag(benchmark,N,N+1),
        N = 1000,
        !.

benchmark :- time(benchmark1).