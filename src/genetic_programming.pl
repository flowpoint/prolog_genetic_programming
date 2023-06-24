:- module(genetic_programming, [genetic_programming/3, selection/4, run_evolution/5]).
:- use_module(library(dialect/xsb/source)).
:- use_module(optimizer).
:- use_module(tasks).
:- use_module(core).

%-----------------------------------------------------------------------------------------------------------------------
% Genetic Programming Module
genetic_programming(Taskname, Optimizername, EvolutionHistory) :-
    run_evolution(Taskname, Optimizername, _, EvolutionHistory, "stopcondition"), 
    !.

%-----------------------------------------------------------------------------------------------------------------------
% Run Evolution with Stop Condition
run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    Result,
    "stopcondition"
    ):-
        EvolutionHistory = [L | _],
        % Print Last Epoch
        writeln(L),
        (
            % If Stop Condition is met, return the Evolution History
            (
                task(Taskname, Costfn, [InitialEpoch], StopCondition),
                EvolutionHistory = [LastEpoch | _],
                Result = EvolutionHistory,
                stopcondition(StopCondition, Costfn, LastEpoch),
                !
            );
            % Else, continue the evolution with Select
            (
            task(Taskname, Costfn, [InitialEpoch], StopCondition),
            (append(_, [InitialEpoch], EvolutionHistory), !),
            run_evolution(
                Taskname,
                Optimizername,
                EvolutionHistory,
                Result,
                "select"
                )
            )
        ).

%-----------------------------------------------------------------------------------------------------------------------
% Run Evolution with Select
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
        % Run Evolution with Crossover
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "crossover"
            ).

%-----------------------------------------------------------------------------------------------------------------------
% Run Evolution with Crossover
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
        % Run Evolution with Mutate
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "mutate"
            ).

%-----------------------------------------------------------------------------------------------------------------------
% Run Evolution with Mutate
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
        % Run Evolution with Stop Condition (Recursion)
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "stopcondition"
            ).
