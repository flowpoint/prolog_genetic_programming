:- begin_tests(genetic_programming).
:- use_module(genetic_programming).

% we start by learning to overfit on the string "Hello world" 

test(run_evolution) :-
    run_evolution(
        [_, _, Initializer, _],
        _,
        _
        ),
    Initializer = ["Hello world", "BBBB"].


task(["Learn String",_,_,_]).

% test that the "Learn String" task is solved optimally
test(genetic_programming) :-
    genetic_programming(Task, _, [LastEpoch | _ ]),
    task(Task),
    member("Hello world", LastEpoch).

:- end_tests(genetic_programming).
