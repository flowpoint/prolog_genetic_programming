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

% Sort a list of genes by their cost
sort_genes_list(Genes, SortedGenes) :-
    predsort(compare_genes, Genes, SortedGenes).

% Comparison predicate for sorting genes based on their cost
% Constructor has to be adjusted to selection operator
compare_genes(Order, gene(_, _, Cost1), gene(_, _, Cost2)) :-
    compare(Order, Cost1, Cost2).
