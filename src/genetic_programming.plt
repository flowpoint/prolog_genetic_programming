:- begin_tests(genetic_programming).
:- use_module(genetic_programming).
:- use_module(core).
:- use_module(tasks).

:- load_test_files(optimizer).
:- load_test_files(tasks).

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

test(run_evolution) :-
   Taskname = "Learn String",
   run_evolution(Taskname, "stringopt", [["Hello world"], ["", ""]], "stopcondition").




:- end_tests(genetic_programming).
