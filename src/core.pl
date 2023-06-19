:- module(core, [gene/1, population/1, epoch/2, evolutionhistory/1, take/3]).

gene(X) :- string(X).

population(X) :- 
    is_list(X),
    maplist(gene, X).

% metrics are stats we want to save like costs and what evolution step we are at
epoch(X, Metrics) :-
    Metrics = [],
    population(X).

evolutionhistory(X) :-
    is_list(X),
    maplist(population, X).

% originating from: https://stackoverflow.com/questions/27151274/prolog-take-the-first-n-elements-of-a-list
take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N - 1, take(M, Xs, Ys).