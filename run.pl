:- initialization(main,main).


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
    halt.


tests :-
    test(X),
    halt.
