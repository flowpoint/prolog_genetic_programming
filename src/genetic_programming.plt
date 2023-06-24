:- begin_tests(genetic_programming).
:- use_module(genetic_programming).
:- use_module(core).
:- use_module(tasks).

:- load_test_files(optimizer).
:- load_test_files(tasks).

%-----------------------------------------------------------------------------------------------------------------------
% Tests for genetic_programming.pl

% Test for genetic_programming/3 with task "Learn_String_with_levenshtein" and optimizer "stringopt"
test(genetic_programming) :-
   Taskname = "Learn_String_with_levenshtein",
   Optimizername = "stringopt",
   genetic_programming(Taskname, Optimizername, [LastEpoch | _ ]),
   member("Hello", LastEpoch).

% Test for genetic_programming/3 with task "Learn_String_with_qudratic" and optimizer "stringopt"
test(genetic_programming) :-
   Taskname = "Learn_String_with_qudratic",
   Optimizername = "stringopt",
   genetic_programming(Taskname, Optimizername, [LastEpoch | _ ]),
   member("Hello", LastEpoch).

%test(run_evolution) :-
%   Taskname = "Learn_String_with_levenshtein",
%   run_evolution(Taskname, _, _,_, "stopcondition").

%test(run_evolution_stop) :-
%   Taskname = "Learn_String_with_levenshtein",
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