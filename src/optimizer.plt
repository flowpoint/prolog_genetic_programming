/** <module> Optimizer Test Module
Tetst for optimizer.pl
@author flowpoint,shinpanse
@license GPL-3.0
*/
:- begin_tests(optimizer).
:- use_module(genetic_programming).
:- use_module(core).
:- use_module(tasks).
:- use_module(optimizer).

% Levenshtein Sort: Hello -> 2 -> 3
test(selection_1) :-
    selection("top10", "levenshtein", [["Hello", "2","3"]], X),
    X = [["Hello","2", "3"],["Hello","2","3"]].
% Levenshtein Sort: 1 -> 2 -> 3
test(selection_2) :-
    selection("top10", "levenshtein", [["1", "2","3"]], X),
    X = [["1","2","3"],["1","2","3"]].
% Levenshtein Sort: Empty List
test(selection_3) :-
    X = [[]],
    \+ selection("top10", "levenshtein", [], X).
% Quadratic Cost Sort: Hello -> 3 -> 2
test(selection_4) :-
    selection("top10", "quadratic_cost", [["Hello", "2","3"]], X),
    X = [["Hello","3", "2"],["Hello","2","3"]].
% Quadratic Cost Sort: 3 -> 2 -> 1
test(selection_5) :-
    selection("top10", "quadratic_cost", [["1", "2","3"]], X),
    X = [["3","2","1"],["1","2","3"]].

% Crossover: headtail -> ANNE, OTTO -> ANTO, OTNE, ANNE, OTTO
test(crossover) :-
    crossover("headtail", [["ANNE", "OTTO"]], [["ANTO", "OTNE", "ANNE", "OTTO"],["ANNE","OTTO"]]).

:- end_tests(optimizer).