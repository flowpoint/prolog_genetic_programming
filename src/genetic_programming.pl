/** <module> Genetic Programming Module
Genetic Programming: Run Evolution with stop condition, selection, crossover, and mutation.
@author flowpoint,shinpanse
@license GPL-3.0
*/
:- module(genetic_programming, [genetic_programming/3, selection/4, run_evolution/5]).
:- use_module(library(dialect/xsb/source)).
:- use_module(optimizer).
:- use_module(tasks).
:- use_module(core).

:- multifile prolog:message//1.

prolog:message(log_genes(Genes)) -->
    {},
    ['Genes: ~w'-[Genes]].

/** Genetic Programming Main Function
 * @param Taskname Name of the Task
 * @param Optimizername Name of the Optimizer
 * @param EvolutionHistory Evolution History
 * @return Result Result of the Evolution
*/ 
genetic_programming(Taskname, Optimizername, EvolutionHistory) :-
    run_evolution(Taskname, Optimizername, _, EvolutionHistory, "stopcondition"), 
    !.

/** Run Evolution of one Epoch
 * @param Taskname Name of the Task
 * @param Optimizername Name of the Optimizer
 * @param EvolutionHistory Evolution History
 * @param Result Result of the Evolution
 * @return Result of the Evolution, when Stop Condition is met run Evolution with Select.
*/
run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    Result,
    "stopcondition"
    ):-
        EvolutionHistory = [L | _],
        % Print Last Epoch
        print_message(informational, log_genes(L)),
        (
            % If Stop Condition is met, return the Evolution History
            (
                task(Taskname, Costfn, _, StopCondition),
                EvolutionHistory = [LastEpoch | _],
                Result = EvolutionHistory,
                stopcondition(StopCondition, Costfn, LastEpoch),
                !
            );
            % Else, continue the evolution with Select
            (
            task(Taskname, _, [InitialEpoch], _),
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

/** Run Evolution with Select
 * @param Taskname Name of the Task
 * @param Optimizername Name of the Optimizer
 * @param EvolutionHistory Evolution History
 * @param Result Result of the Evolution
 * @return Run Evolution with Crossover
*/
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

/** Run Evolution with Crossover
 * @param Taskname Name of the Task
 * @param Optimizername Name of the Optimizer
 * @param EvolutionHistory Evolution History
 * @param Result Result of the Evolution
 * @return Run Evolution with Mutate
*/
run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    Result,
    "crossover"
    ):-
        task(Taskname, _, _, _),
        optimizer(Optimizername, _, Crossoverop, _),
        crossover(Crossoverop, EvolutionHistory, NewEvolutionHistory),
        % Run Evolution with Mutate
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "mutate"
            ).


/** Run Evolution with Mutate
 * @param Taskname Name of the Task
 * @param Optimizername Name of the Optimizer
 * @param EvolutionHistory Evolution History
 * @param Result Result of the Evolution
 * @return Run Evolution with Stop Condition
 * @note This is the last step of the Evolution
*/
run_evolution(
    Taskname,
    Optimizername,
    EvolutionHistory,
    Result,
    "mutate"
    ):-
        task(Taskname, _, _, _),
        optimizer(Optimizername, _, _, Mutationop),
        mutate(Mutationop, EvolutionHistory, NewEvolutionHistory),
        % Run Evolution with Stop Condition (Recursion)
        run_evolution(
            Taskname,
            Optimizername,
            NewEvolutionHistory,
            Result,
            "stopcondition"
            ).
