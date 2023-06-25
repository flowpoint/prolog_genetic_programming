:- initialization(main,main).
:- use_module(src/genetic_programming).

run_task(Argv) :-
    Argv = [ Taskname | [Optimizername]],
    atom_string(Taskname, Tn),
    atom_string(Optimizername, On),
    genetic_programming:genetic_programming(Tn, On, [FinalGenes| _]).

main(Argv) :- 
    run_task(Argv),
    halt.


