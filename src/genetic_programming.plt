:- begin_tests(genetic_programming).
:- use_module(genetic_programming).
:- use_module(core).
:- use_module(tasks).


% we start by learning to overfit on the string "Hello world" 

% test that the "Learn String" task is solved optimally, 
% by the occurence of the string in the last epochs genes
test(genetic_programming) :-
   Taskname = "Learn String",
   genetic_programming(Taskname, _, [LastEpoch | _ ]),
   member("Hello world", LastEpoch).


test(run_evolution) :-
   Taskname = "Learn String",
   run_evolution(Taskname, _, _, "stopcondition").


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
