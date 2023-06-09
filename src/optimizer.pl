/** <module> Optimizer module
This module contains the optimizer module, which is responsible for the optimization of the population.
@author flowpoint,shinpanse
@license GPL-3.0
*/
:- module(optimizer, [optimizer/4, selection/4, crossover/3, mutate/3, crossover/3, cross/4, cross2/2, split_list/4, mutate_pos/4, split_string_pos/4]).
:- use_module(library(pairs)).
:- use_module(core).
:- use_module(tasks).

/** Main optimizer predicate
 * @param Unique Identifier of the optimizer
*/
:- dynamic optimizer/4.

:- asserta(
(
optimizer("stringopt", Selectionop, Crossoverop, Mutationop) :-
    Selectionop="top10",
    Crossoverop="headtail",
    Mutationop="indel",
    selection(Selectionop, _, _, _),
    crossover(Crossoverop, _, _),
    mutate(Mutationop, _, _),
    !
)
).

/** Selection operator 
 * Sorts the population based on the cost function and selects the top 10
 * @param Unique Identifier of the selection operator
 * @param Cost function
 * @param Evolution history
 * @param New evolution history
*/
:- dynamic selection/4.

:- asserta(
(
selection("top10", Costfn, [LastEpoch | Prev], [NewPopulation | [LastEpoch | Prev] ]) :-
    mapcost(Costfn, LastEpoch, Costs),
    pairs_keys_values(
        Pairs, 
        Costs,
        LastEpoch),
    keysort(Pairs, Sorted),
    take(10, Sorted, NewPopulationKV),
    pairs_values(NewPopulationKV, NewPopulation),
    !
)
).

:- asserta(
(
selection("top1k", Costfn, [LastEpoch | Prev], [NewPopulation | [LastEpoch | Prev] ]) :-
    mapcost(Costfn, LastEpoch, Costs),
    pairs_keys_values(
        Pairs, 
        Costs,
        LastEpoch),
    keysort(Pairs, Sorted),
    take(1000, Sorted, NewPopulationKV),
    pairs_values(NewPopulationKV, NewPopulation),
    !
)
).

/** Crossover operator
 * Splits the string in half and swaps the halves between two strings
*/
string_halves(S, Half1, Half2) :-
    string_length(S,Sl),
    Slh is Sl // 2,
    sub_string(S, 0, Slh, _, Half1),
    sub_string(S, Slh, _, 0, Half2).

cross(GeneA, GeneB, NewGeneA, NewGeneB):-
    string_halves(GeneA, GA1, GA2),
    string_halves(GeneB, GB1, GB2),
    string_concat(GA1,GB2,NewGeneA),
    string_concat(GB1,GA2,NewGeneB),
    !.

cross2([], []).
cross2([A], [A]).
cross2(Genes, NewGenes) :-
   Genes = [GeneA | [ GeneB | R]],
   NewGenes = [GeneNA | [ GeneNB | NR]],
   cross(GeneA,GeneB, GeneNA, GeneNB),
   cross2(R,NR).

:- dynamic crossover/3.

:- asserta(
(
crossover("headtail", EvolutionHistory, NewEvolutionHistory) :-
    EvolutionHistory = [LastEpoch | _],
    NewEvolutionHistory = [NewEpoch | EvolutionHistory],
    cross2(LastEpoch, Crossed_Epoch),
    append(Crossed_Epoch, LastEpoch, NewEpoch),
    !
)
).

/** Mutation operator
 * Mutates the string by inserting, deleting or replacing a character
*/
split_list(Index, List, Split1, Split2) :-
    length(Split1, Index),
    append(Split1, Split2, List).

split_string_pos(Index, String, Split1, Split2) :-
    string_chars(String, Chars),
    split_list(Index,Chars,Split1,Split2).

:- dynamic symbols/1.

:- asserta(
(
symbols(L) :-
    string_chars(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ 1234567890",
    L)
)
).

charo(Char) :-
    symbols(L),
    string_chars(Char,C),
    member(C, L).

random_char(Char) :-
    symbols(L),
    random_member(Char, L),
    !.

mutate_pos("insertion", Index, Gene, NewGene) :-
    random_char(E),
    nth0(Index, NewChars, E, Chars),
    string_chars(Gene, Chars),
    string_chars(NewGene, NewChars),
    !.

mutate_pos("deletion", _, "", "").
mutate_pos("deletion", Index, Gene, NewGene) :-
    string_chars(Gene,Chars),
    nth0(Index, Chars, _, NewChars),
    string_chars(NewGene,NewChars),
    !.

mutate_pos("replacement", _, "", "").
mutate_pos("replacement", Index, Gene, NewGene) :-
    random_char(E),
    nth0(Index, Chars, _, MidGene),
    nth0(Index, NewChars, E, MidChars),
    string_chars(Gene, Chars),
    string_chars(MidGene, MidChars),
    string_chars(NewGene, NewChars),
    !.

mutate_gene(Gene, NewGene) :-
    gene(Gene),
    string_length(Gene,0),
    mutate_pos("insertion", 0, Gene, NewGene),
    !.

:- dynamic mutate_gene/2.

:- asserta(
(
mutate_gene(Gene, NewGene) :-
    gene(Gene),
    string_length(Gene, L),
    K is L-1,
    random_between(0, K, Index),
    random_member(Indel, ["insertion","deletion","replacement"]),
    mutate_pos(Indel, Index, Gene, NewGene),
    !
)
).

map_mutate_gene([],[]).
map_mutate_gene([G | R], [NewG | NR]):-
    mutate_gene(G, NewG),
    map_mutate_gene(R,NR).

:- dynamic mutate/3.

:- asserta(
(
mutate("indel", EvolutionHistory, NewHistory) :-
    EvolutionHistory = [LastEpoch | _],
    NewHistory = [ NewEpoch | EvolutionHistory],
    map_mutate_gene(LastEpoch, Mutated_epoch),
    append(Mutated_epoch, LastEpoch, NewEpoch),
    !
)
).
