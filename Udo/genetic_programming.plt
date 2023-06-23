:- begin_tests(genetic_programming).
:- use_module(genetic_programming).
:- use_module(core).
:- use_module(task).
:- use_module(cost_function).

:- load_test_files(optimizer).
:- load_test_files(cost_function).


test(genetic_programming) :-
   Taskname = "Learn_String_with_Levenshtein",
   Optimizername = "stringopt",
   genetic_programming(Taskname, Optimizername, [LastEpoch | _ ]),
   member("Hello", LastEpoch).

%test(run_evolution) :-
%  Taskname = "Learn String",
%   run_evolution(Taskname, _, _,_, "stopcondition").

%test(run_evolution_stop) :-
%   Taskname = "Learn String",
%   run_evolution(Taskname, "stringopt", _, [["Hello"], ["", ""]], "stopcondition").

%test(genetic_programming) :-
%   Taskname = "TakeMe",
%   Optimizername = "stringopt",
%   genetic_programming(Taskname, Optimizername, [LastEpoch | _ ]),
%   member("Hello", LastEpoch).
   
%test(run_evolution) :-
%   Taskname = "TakeMe",
%   run_evolution(Taskname, _, _,_, "stopcondition").
   
%test(run_evolution_stop) :-
%   Taskname = "TakeMe",
%   run_evolution(Taskname, "stringopt", _, [["Hello], ["", ""]], "stopcondition").

:- end_tests(genetic_programming).