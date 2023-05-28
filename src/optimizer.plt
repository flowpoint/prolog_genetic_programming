:- begin_tests(optimizer).
:- use_module(genetic_programming).
:- use_module(core).
:- use_module(tasks).

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

:- end_tests(optimizer).
