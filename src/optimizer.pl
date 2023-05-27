:- module(optimizer, [optimizer/4, selection/4, crossover/3, mutate/3]).
:- use_module(core).
:- use_module(tasks).

selection("top2", [LastEpoch | Prev], Costfn, [NewPopulation | [LastEpoch | Prev] ]) :-
    mapcost(Costfn, LastEpoch, Costs),
    pairs_keys_values(
        Pairs, 
        Costs,
        LastEpoch),
    keysort(Pairs, Sorted),
    take(2, Sorted, NewPopulationKV),
    pairs_values(NewPopulationKV, NewPopulation).

crossover("none", [LastEpoch | _], LastEpoch).
mutate("none", [LastEpoch | _], LastEpoch).

% optimizername, selectionop, crossoverop, mutationsop
optimizer("stringopt", Selectionop, Crossoverop, Mutationop) :-
    selection(Selectionop, _, _, _),
    crossover(Crossoverop, _, _),
    mutate(Mutationop, _, _).

