:- initialization(main,main).


echo([]):- nl.

echo([Last]):- !,
    write(Last), nl.

echo([H|T]) :-
    write(H), write(' '),
    echo(T).

main(Argv) :- 
    echo(Argv),
    halt.

