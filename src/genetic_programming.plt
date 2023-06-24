/** <module> Genetic Programming Test Module
Tetst for genetic_programming.pl
@author flowpoint,shinpanse
@license GPL-3.0
*/
:- begin_tests(genetic_programming).
:- use_module(genetic_programming).
:- use_module(core).
:- use_module(tasks).

% Loading test files for optimizer.pl and tasks.pl
:- load_test_files(optimizer).
:- load_test_files(tasks).

% Test for genetic_programming/3 with task "Learn_String_with_levenshtein" and optimizer "stringopt"
test(genetic_programming) :-
   Taskname = "Learn_String_with_levenshtein",
   Optimizername = "stringopt",
   genetic_programming(Taskname, Optimizername, [LastEpoch | _ ]),
   member("Hello", LastEpoch).

test(run_evolution) :-
   Taskname = "Learn_String_with_levenshtein",
   run_evolution(Taskname, _, _,_, "stopcondition").

test(run_evolution_stop) :-
   Taskname = "Learn_String_with_levenshtein",
   run_evolution(Taskname, "stringopt", _, [["Hello"], ["", ""]], "stopcondition").

% Test for genetic_programming/3 with task "Learn_String_with_quadratic" and optimizer "stringopt"
test(genetic_programming) :-
   Taskname = "Learn_String_with_quadratic",
   Optimizername = "stringopt",
   genetic_programming(Taskname, Optimizername, [LastEpoch | _ ]),
   member("Hello", LastEpoch).

test(run_evolution) :-
   Taskname = "Learn_String_with_quadratic",
   run_evolution(Taskname, _, _,_, "stopcondition").

test(run_evolution_stop) :-
   Taskname = "Learn_String_with_quadratic",
   run_evolution(Taskname, "stringopt", _, [["Hello"], ["", ""]], "stopcondition").


:- end_tests(genetic_programming).