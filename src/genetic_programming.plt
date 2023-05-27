:- begin_tests(genetic_programming).
:- use_module(genetic_programming).
:- use_module(tasks).

% we start by learning to overfit on the string "Hello world" 

% test(run_evolution) :-
%     run_evolution(
%         [_, _, Initializer, _],
%         _,
%         _
%         ),
%     Initializer = ["Hello world", "BBBB"].


task(["Learn String",_,_,_]).

% test that the "Learn String" task is solved optimally
% test(genetic_programming) :-
%    genetic_programming(Task, _, [LastEpoch | _ ]),
%    task(Task).
%    %member("Hello world", LastEpoch).


test(run_evolution) :-
    task(Task),
    run_evolution(Task, _, _, "stopcondition").


test(selection) :-
    selection("top2", [["Hello world", "2","3"]], "accuracy", X),
    X = [["Hello world","2"],["Hello world","2","3"]].

test(selection) :-
    selection("top2", [["1", "2","3"]], "accuracy", X),
    X = [["1","2"],["1","2","3"]].

% for when we have ops that add can add genes, and we start with 0 genes, selection should still work
test(selection) :-
    selection("top2", [[]], "accuracy", X),
    X = [[],[]].

% should fail cause, evolutionhistory is not correct
test(selection) :-
    X = [[]],
    \+ selection("top2", [], "accuracy", X).


test(mapcost) :-
    mapcost("accuracy", ["Hello world","2"], [0,1]).

:- end_tests(genetic_programming).
