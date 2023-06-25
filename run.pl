:- initialization(main,main).
:- use_module(src/genetic_programming).


echo([]):- nl.

echo([Last]):- !,
    write(Last), nl.

echo([H|T]) :-
    write(H), write(' '),
    echo(T).

tests(X) :-
    echo(X).

main(Argv) :- 
    echo(Argv),
    genetic_programming:run_example(Argv),
    halt.


