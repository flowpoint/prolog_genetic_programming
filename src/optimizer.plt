:- begin_tests(optimizer).
:- use_module(genetic_programming).
:- use_module(core).
:- use_module(tasks).
:- use_module(optimizer).

test(selection_1) :-
    selection("top10", "levenshtein", [["Hello", "2","3"]], X),
    X = [["Hello","2", "3"],["Hello","2","3"]].

test(selection_2) :-
    selection("top10", "levenshtein", [["1", "2","3"]], X),
    X = [["1","2","3"],["1","2","3"]].

% for when we have ops that add can add genes, and we start with 0 genes, selection should still work

% should fail cause, evolutionhistory is not correct
test(selection_3) :-
    X = [[]],
    \+ selection("top10", "levenshtein", [], X).

test(crossover) :-
    crossover("headtail", [["ANNE", "OTTO"]], [["ANTO", "OTNE", "ANNE", "OTTO"],["ANNE","OTTO"]]).

:- end_tests(optimizer).