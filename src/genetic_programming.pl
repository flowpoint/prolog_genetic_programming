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
        ( 
        task(Taskname, Costfn, [Initializer], StopCondition),
        EvolutionHistory = [LastEpoch | _],
        stopcondition(StopCondition, Costfn, LastEpoch),
        !
        )
        ;
        (
        task(Taskname, Costfn, [Initializer], StopCondition),
        (append(_, [Initializer], EvolutionHistory),!),
        run_evolution(
            Taskname,
            Optimizername,
            EvolutionHistory,
            "select"
            )
        ).

run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    "select"
    ):-
        task(Taskname, Costfn, _, _),
        optimizer(Optimizername, Selectionop, _, _),
        selection(Selectionop, Costfn, EvolutionHistory, NewEvolutionHistory), 
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            "crossover"
            ).

run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    "crossover"
    ):-
        EvolutionHistory = [L | _],
        writeln(L),
        task(Taskname, Costfn, _, _),
        optimizer(Optimizername, Selectionop, Crossoverop, Mutationop),
        crossover(Crossoverop, EvolutionHistory, NewEvolutionHistory),
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            "mutate"
            ).

run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    "mutate"
    ):-
        task(Taskname, Costfn, _, _),
        optimizer(Optimizername, Selectionop, Crossoverop, Mutationop),
        mutate(Mutationop, EvolutionHistory, NewEvolutionHistory),
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            "stopcondition"
            ).


genetic_programming(Taskname, Optimizername, EvolutionHistory) :-
    run_evolution(Taskname, Optimizername, EvolutionHistory, "stopcondition").

    
run_example(Taskname):-
    Taskname="Learn String",
    Optimizername="stringopt",
    genetic_programming(Taskname, Optimizername, X),
    write(X).
