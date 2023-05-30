:- module(optimizer, [optimizer/4, selection/4, crossover/3, mutate/3, crossover/3, cross/4, cross2/2, split_list/4, mutate_pos/4, split_string_pos/4]).

:- use_module(library(pairs)).
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

string_halves(S, Half1, Half2) :-
    string_length(S,Sl),
    Slh is Sl // 2,
    sub_string(S, 0, Slh, _, Half1),
    sub_string(S, Slh, _, 0, Half2).

cross(GeneA, GeneB, NewGeneA, NewGeneB):-
    string_halves(GeneA, GA1, GA2),
    string_halves(GeneB, GB1, GB2),
    string_concat(GA1,GB2,NewGeneA),
    string_concat(GA2,GB1,NewGeneB),
    !.

cross2([], []).
cross2([A], [A]).
cross2(Genes, NewGenes) :-
   Genes = [GeneA | [ GeneB | R]],
   NewGenes = [GeneNA | [ GeneNB | NR]],
   cross(GeneA,GeneB, GeneNA, GeneNB),
   cross2(R,NR).

crossover("none", [LastEpoch | _], NewHistory) :-
    cross2(LastEpoch, NewHistory).

% crossover("neighbors_splice", [LastEpoch | _], NewHistory) :-

split_list(Index, List, Split1, Split2) :-
    length(Split1, Index),
    append(Split1, Split2, List).

split_string_pos(Index, String, Split1, Split2) :-
    string_chars(String, Chars),
    %string_chars(Split1,S1),
    %string_chars(Split2,s2),
    split_list(Index,Chars,Split1,Split2).


charo(Char) :-
    string_chars(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    L),
    string_chars(Char,C),
    member(C, L).

random_char(Char) :-
    string_chars(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    L),
    random_member(Char, L),
    !
    .
% mutateGene(Gene, NewGene) :-
%     string_length(Gene, L),
%     random_between(0,L, I),
%     select(Gene, 
%
% mutate_pos("insertion", Index, Gene, NewGene) :-
%     split_string_pos(Index, Gene, Split1, Split2),
%     random_char(N),
%     append([Split1, [N], Split2], NewGene).
%

mutate_pos("insertion", Index, Gene, NewGene) :-
    random_char(E),
    nth0(Index, NewChars, E, Chars),
    string_chars(Gene, Chars),
    string_chars(NewGene, NewChars),
    !.

mutate_pos("deletion", Index, Gene, NewGene) :-
    string_chars(Gene,Chars),
    nth0(Index, Chars, _, NewChars),
    string_chars(NewGene,NewChars),
    !.

mutate_pos("replacement", Index, Gene, NewGene) :-
    random_char(E),
    nth0(Index, Chars, _, MidGene),
    nth0(Index, NewChars, E, MidChars),
    string_chars(Gene, Chars),
    string_chars(MidGene, MidChars),
    string_chars(NewGene, NewChars),
    !.

% mutatePos("insertion", Index, Gene, NewGene) :-
%     split_list(Index, Gene, [Split1], Split2),
%     random_char(N),
%     NewGene = [ Split1 | [ N | Split2 ]].


mutate_gene(Gene, NewGene) :-
    string_length(Gene, L),
    random_between(0, L, Index),
    random_member(Indel, ["insertion","deletion","replacement"]),
    mutate_pos(Indel, Index, Gene, NewGene),
    !.

map_mutate_gene([],[]).
map_mutate_gene([G | R], [NewG | NR]):-
    mutate_gene(G, NewG),
    map_mutate_gene(R,NR).

mutate("none", [LastEpoch | _], NewHistory) :-
    map_mutate_gene(LastEpoch, NewHistory).

% optimizername, selectionop, crossoverop, mutationsop
optimizer("stringopt", Selectionop, Crossoverop, Mutationop) :-
    selection(Selectionop, _, _, _),
    crossover(Crossoverop, _, _),
    mutate(Mutationop, _, _).

