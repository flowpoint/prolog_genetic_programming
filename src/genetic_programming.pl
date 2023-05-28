:- module(genetic_programming, [genetic_programming/3, selection/4, run_evolution/4]).
:- use_module(library(dialect/xsb/source)).
:- use_module(optimizer).
:- use_module(tasks).
:- use_module(core).

run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    "stopcondition"
    ):-
        ( EvolutionHistory = [LastEpoch | _],
        task(Taskname, Costfn, _, StopCondition),
        stopcondition(StopCondition, Costfn, LastEpoch)
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

    
run_example(Taskname):-
    Taskname="Learn String",
    Optimizername="stringopt",
    genetic_programming(Taskname, Optimizername, X),
    write(X).
