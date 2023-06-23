:- module(genetic_programming, [genetic_programming/3, selection/4, run_evolution/5]).
:- use_module(library(dialect/xsb/source)).
:- use_module(optimizer).
:- use_module(task).
:- use_module(core).
:- use_module(cost_function).

%------------------------------------------------------------------------------------------------------------------------
% Run genetic programming
genetic_programming(Taskname, Optimizername, EvolutionHistory) :-
    run_evolution(Taskname, Optimizername, _, EvolutionHistory, "stopcondition"), 
    !.

%------------------------------------------------------------------------------------------------------------------------
% Run Evolution
run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    Result,
    "stopcondition"
    ):-
        EvolutionHistory = [L | _],
        % Print the last epoch.
        writeln(L),
        (
            % If the stop condition is met, return the evolution history.
            (
                task(Taskname, Costfn, [InitialEpoch], StopCondition),
                EvolutionHistory = [LastEpoch | _],
                Result = EvolutionHistory,
                stopcondition(StopCondition, Costfn, LastEpoch),
                !
            );
            % Otherwise, continue the evolution.
            (
            task(Taskname, Costfn, [InitialEpoch], StopCondition),
            (append(_, [InitialEpoch], EvolutionHistory), !),
            % Run evolution for the next epoch with select optimizer.
            run_evolution(
                Taskname,
                Optimizername,
                EvolutionHistory,
                Result,
                "select"
                )
            )
        ).

%------------------------------------------------------------------------------------------------------------------------
% Selection
run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    Result,
    "select"
    ):-
        task(Taskname, Costfn, _, _),
        optimizer(Optimizername, Selectionop, _, _),
        selection(Selectionop, Costfn, EvolutionHistory, NewEvolutionHistory), 
        % Run evolution for the next epoch with crossover optimizer.
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "crossover"
            ).

%------------------------------------------------------------------------------------------------------------------------
% Crossover
run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    Result,
    "crossover"
    ):-
        task(Taskname, Costfn, _, _),
        optimizer(Optimizername, Selectionop, Crossoverop, Mutationop),
        crossover(Crossoverop, EvolutionHistory, NewEvolutionHistory),
        % Run evolution for the next epoch with mutate optimizer.
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "mutate"
            ).

%------------------------------------------------------------------------------------------------------------------------
% Mutation
run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    Result,
    "mutate"
    ):-
        task(Taskname, Costfn, _, _),
        optimizer(Optimizername, Selectionop, Crossoverop, Mutationop),
        mutate(Mutationop, EvolutionHistory, NewEvolutionHistory),
        % Run evolution for the next epoch with stop condition. (Recursion)
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "stopcondition"
            ).