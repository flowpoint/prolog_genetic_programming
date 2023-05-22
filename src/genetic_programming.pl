:- module(genetic_programming, [genetic_programming/3]).
:- use_module(library(dialect/xsb/source)).


gene(X) :- string(X).

population(X) :- 
    is_list(X),
    maplist(gene, X).

epoch(X) :-
    population(X).

evolutionHistory(X) :-
    is_list(X),
    maplist(epoch, X).

learn_string_task([
    "Learn String", 
    "accuracy",
    ["AAAA","BBBBB"],
    "zero_cost"
    ]):-
        true.


task([TaskName, Costfn, Initializer, StopCondition]) :-
    % unique identifier to summarize the task
    TaskName = _,
    % cost function describes what the tasks goal is
    % by assigning a cost value to the "solutions"
    Costfn = _,
    % initializer describes the initial state the task should be started from
    % this is part of the task, so the optimizers can be agnostic to the tasks
    % additionally the task definition might contain realistically an inital 
    % state to be started from
    Initializer = _,
    % the stopcondition defines when a task is achieved
    StopCondition = _.



%levenshtein(A,B, Cost).

costfn("accuracy", Gene, Cost) :-
    (Cost = 0, Gene = "Hello world", !);
    Cost = 1.


stopCondition("zero_cost", Costfn, LastEpoch) :-
    costfn(Costfn, LastEpoch, Cost),
    Cost = 0.

% zero_cost
%stopCondition("zero_cost", Costfn, [ LastEpoch | _ ]) :-
%costfn(Costfn, LastEpoch, Cost),
%Cost = 0.


% originating from: https://stackoverflow.com/questions/27151274/prolog-take-the-first-n-elements-of-a-list
take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N - 1, take(M, Xs, Ys).

costfnp(Costfn, Population, Cost) :-
    call(costfn(Costfn), Population, Cost).

mapcost(_, [], []).
mapcost(Costfn, [H|T], [Cost|CT]):-
    costfn(Costfn, H, Cost),
    mapcost(Costfn, T, CT).

kvs(Costfn, LastEpoch, Pairs) :-
    pairs_keys_values(
        Pairs,
        Costs,
        LastEpoch),
    mapcost(Costfn, LastEpoch, Costs).

selection("top2", [LastEpoch | Prev], Costfn, [NewPopulation, [LastEpoch, Prev] ]) :-
    keysort(Sorted, Pairs),
    mapcost(Costfn, LastEpoch, Costs),
    pairs_keys_values(
        Pairs, 
        Costs,
        LastEpoch),
    take(2, Sorted, NewPopulation).


crossover("none", [LastEpoch | _], LastEpoch).
mutate("none", [LastEpoch | _], LastEpoch).

optimizer([Selectionop, Crossoverop, Mutationop]) :-
    selection(Selectionop, _, _),
    crossover(Crossoverop, _, _),
    mutate(Mutationop, _, _).


run_evolution(
    [_, Costfn, Initializer, StopCondition], 
    [Selectionop, Crossoverop, Mutationop], 
    EvolutionHistory,
    "stopcondition"
    ):-
        stopCondition(StopCondition, Costfn, EvolutionHistory);
        run_evolution(
            [_, Costfn, Initializer, StopCondition], 
            [Selectionop, Crossoverop, Mutationop], 
            EvolutionHistory,
            "select"
            ).

run_evolution(
    [_, Costfn, Initializer, StopCondition], 
    [Selectionop, Crossoverop, Mutationop], 
    EvolutionHistory,
    "select"
    ):-
        selection(Selectionop, EvolutionHistory, Costfn, NewHistory),
        run_evolution(
            [_, Costfn, Initializer, StopCondition], 
            [Selectionop, Crossoverop, Mutationop], 
            NewHistory,
            "crossover"
            ).

run_evolution(
    [_, Costfn, Initializer, StopCondition], 
    [Selectionop, Crossoverop, Mutationop], 
    EvolutionHistory,
    "crossover"
    ):-
        crossover(Selectionop, EvolutionHistory, Costfn, NewHistory),
        run_evolution(
            [_, Costfn, Initializer, StopCondition], 
            [Selectionop, Crossoverop, Mutationop], 
            NewHistory,
            "mutate"
            ).

run_evolution(
    [_, Costfn, Initializer, StopCondition], 
    [Selectionop, Crossoverop, Mutationop], 
    EvolutionHistory,
    "mutate"
    ):-
        mutate(Selectionop, EvolutionHistory, Costfn, NewHistory),
        run_evolution(
            [_, Costfn, Initializer, StopCondition], 
            [Selectionop, Crossoverop, Mutationop], 
            NewHistory,
            "stopcondition"
            ).


genetic_programming(Task, Optimizer, EvolutionHistory) :-
    learn_string_task(Task),
    %optimizer(Optimizer),
    run_evolution(Task, Optimizer, EvolutionHistory, "stopcondition").

    
run_example(X):-
    (X="Learn String", 
        genetic_programming(["Learn String"|_], _, ["Hello world"])).
