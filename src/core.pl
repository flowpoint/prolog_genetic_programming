:- module(core, [gene/1, population/1, epoch/2, evolutionhistory/1, take/3]).

% The predicate gene/1 is defined to be true when the agrument 'X' is a string.
gene(X) :- string(X).

% The predicate population/1 is defined to be true when the argument 'X' is a list of strings.
population(X) :- 
    is_list(X),
    maplist(gene, X).

% The predicate epoch/2 is defined to be true when the argument 'X' is a list of strings and the 
% argument 'Metrics' is a list of lists of strings.
epoch(X, Metrics) :-
    Metrics = [],
    population(X).

% The predicate evolutionhistory/1 is defined to be true when the argument X is a list and all 
% its elements satisfy the epoch/2 predicate. This predicate ensures that X represents a valid history of evolution.
evolutionhistory(X) :-
    is_list(X),
    maplist(epoch, X).

% Originating from: https://stackoverflow.com/questions/27151274/prolog-take-the-first-n-elements-of-a-list
take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N - 1, take(M, Xs, Ys).
