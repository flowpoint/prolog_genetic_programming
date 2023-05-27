:- module(genetic_programming, [genetic_programming/3, mapcost/3, selection/4, run_evolution/4]).
:- use_module(library(dialect/xsb/source)).
:- use_module(tasks).


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
    maplist(epoch, X).

    % task([TaskName, Costfn, Initializer, StopCondition]) :-
    %     % unique identifier to summarize the task
    %     TaskName = _,
    %     % cost function describes what the tasks goal is
    %     % by assigning a cost value to the "solutions"
    %     Costfn = _,
    %     % initializer describes the initial state the task should be started from
    %     % this is part of the task, so the optimizers can be agnostic to the tasks
    %     % additionally the task definition might contain realistically an inital 
    %     % state to be started from
    %     Initializer = _,
    %     % the stopcondition defines when a task is achieved
    %     StopCondition = _.



%levenshtein(A,B, Cost).

% per gene cost fn
costfn("accuracy", Gene, Cost) :-
    (Cost = 0, Gene = "Hello world");
    Cost = 1.

% costfn("abs", Gene, Cost) :-
%    atom_number(Gene, Cost).


% originating from: https://stackoverflow.com/questions/27151274/prolog-take-the-first-n-elements-of-a-list
take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N - 1, take(M, Xs, Ys).

% costfnp(Costfn, Population, Cost) :-
% call(costfn(Costfn), Population, Cost).

mapcost(_, [], []).
mapcost(Costfn, [H|T], [Cost|CT]):-
    costfn(Costfn, H, Cost),
    mapcost(Costfn, T, CT).


stopCondition("zero_cost", Costfn, LastEpoch) :-
    %costfn(Costfn, LastEpoch, Costs),
    mapcost(Costfn, LastEpoch, Costs),
    member(0, Costs).

% zero_cost
%stopCondition("zero_cost", Costfn, [ LastEpoch | _ ]) :-
%costfn(Costfn, LastEpoch, Cost),
%Cost = 0.

kvs(Costfn, LastEpoch, Pairs) :-
    pairs_keys_values(
        Pairs,
        Costs,
        LastEpoch),
    mapcost(Costfn, LastEpoch, Costs).

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


run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    "stopcondition"
    ):-
        ( EvolutionHistory = [LastEpoch | _],
        task(Taskname, Costfn, _, StopCondition),
        stopCondition(StopCondition, Costfn, LastEpoch)
        );
        run_evolution(
            Taskname,
            Optimizername,
            EvolutionHistory,
            "select"
            ).

run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    "select"
    ):-
        task(Taskname, Costfn, _, _),
        optimizer(Optimizername, Selectionop, _, _),
        selection(Selectionop, EvolutionHistory, Costfn, NewHistory),
        run_evolution(
            Taskname,
            Optimizername,
            NewHistory,
            "crossover"
            ).

run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    "crossover"
    ):-
        task(Taskname, Costfn, _, _),
        optimizer(Optimizername, Selectionop, _, _),
        crossover(Selectionop, EvolutionHistory, Costfn, NewHistory),
        run_evolution(
            Taskname,
            Optimizername,
            NewHistory,
            "mutate"
            ).

run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    "mutate"
    ):-
        task(Taskname, Costfn, _, _),
        optimizer(Optimizername, Selectionop, _, _),
        mutate(Selectionop, EvolutionHistory, Costfn, NewHistory),
        run_evolution(
            Taskname,
            Optimizername,
            NewHistory,
            "stopcondition"
            ).


genetic_programming(Taskname, Optimizername, EvolutionHistory) :-
    run_evolution(Taskname, Optimizername, EvolutionHistory, "stopcondition").

    
run_example(X):-
    (Taskname="Learn String", 
    Optimizername="stringopt",
        genetic_programming(Taskname, Optimizername, X)).
