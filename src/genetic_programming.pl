:- module(genetic_programming, [genetic_programming/3, selection/4, run_evolution/5]).
:- use_module(library(dialect/xsb/source)).
:- use_module(optimizer).
:- use_module(tasks).
:- use_module(core).



run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    Result,
    "stopcondition"
    ):-
        EvolutionHistory = [L | _],
        writeln(L),
        (
            (
                task(Taskname, Costfn, [InitialEpoch], StopCondition),
                EvolutionHistory = [LastEpoch | _],
                Result = EvolutionHistory,
                stopcondition(StopCondition, Costfn, LastEpoch),
                !
            );
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
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "crossover"
            ).

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
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "mutate"
            ).

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
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "stopcondition"
            ).


genetic_programming(Taskname, Optimizername, EvolutionHistory) :-
    run_evolution(Taskname, Optimizername, _, EvolutionHistory, "stopcondition"), !.

    
run_example(Taskname):-
    Taskname="Learn String",
    Optimizername="stringopt",
    genetic_programming(Taskname, Optimizername, X),
    write(X).
