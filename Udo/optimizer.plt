:- begin_tests(optimizer).
:- use_module(genetic_programming).
:- use_module(core).
:- use_module(task).
:- use_module(optimizer).
:- use_module(cost_function).

%------------------------------------------------------------------------------------------------------------------------
% Test Selection
test(selection_1) :-
    selection("top10", "levenshtein", [["Hello", "2","3"]], X),
    X = [["Hello","2", "3"],["Hello","2","3"]].

test(selection_2) :-
    selection("top10", "levenshtein", [["1", "2","3"]], X),
    X = [["1","2","3"],["1","2","3"]].

test(selection_3) :-
    selection("top10", "quadratic_cost", [["Hello", "2","3"]], X),
    X = [["Hello","3", "2"],["Hello","2","3"]].

test(selection_4) :-
    selection("top10", "quadratic_cost", [["1", "2","3"]], X),
    X = [["3","2","1"],["1","2","3"]].

test(selection_5) :-
    X = [[]],
    \+ selection("top10", "levenshtein", [], X).

%------------------------------------------------------------------------------------------------------------------------  
% Test Crossover
test(crossover) :-
    crossover("headtail", [["ANNE", "OTTO"]], [["ANTO", "OTNE", "ANNE", "OTTO"],["ANNE","OTTO"]]).

:- end_tests(optimizer).