:- initialization(main,main).
:- use_module(src/genetic_programming).

%opt_type(Flag, Opt, Type) :-


run_task(Argv) :-
    Argv = [ Taskname | [Optimizername]],
    atom_string(Taskname, Tn),
    atom_string(Optimizername, On),
    genetic_programming:genetic_programming(Tn, On, _),
    write("1").

main(Argv) :- 
    run_task(Argv),
    halt.


