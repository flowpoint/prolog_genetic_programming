/** <module> Core Module
Core Structure of the Genetic Programming System
@author flowpoint,shinpanse
@license GPL-3.0
*/
:- module(core, [gene/1, population/1, epoch/2, evolutionhistory/1, take/3]).

% Gene is a string
gene(X) :- string(X).

% Population is a list of genes
population(X) :- 
    is_list(X),
    maplist(gene, X).

% Epoch is a population and a list of metrics
epoch(X, Metrics) :-
    Metrics = [],
    population(X).

% Evolution History is a list of epochs
evolutionhistory(X) :-
    is_list(X),
    maplist(population, X).

% originating from: https://stackoverflow.com/questions/27151274/prolog-take-the-first-n-elements-of-a-list

take(N, _, Xs) :- 
    N =< 0, !, N =:= 0, Xs = [].

take(_, [], []).

take(N, [X|Xs], [X|Ys]) :- 
    M is N - 1, take(M, Xs, Ys).