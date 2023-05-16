:- begin_tests(genetic_programming).
:- use_module(genetic_programming).

% we start by learning to overfit on the string "Hello world" 

task(["Learn String",_,_,_]).

% test that the "Learn String" task is solved optimally
test(genetic_programming) :-
    genetic_programming(Task, _, EvolutionHistory),
    task(Task),
    member("Hello world", EvolutionHistory).

:- end_tests(genetic_programming).
