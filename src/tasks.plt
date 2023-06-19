:- begin_tests(tasks).
:- use_module(core).
:- use_module(tasks).

% all with respect to the current task
% of matching to the string "Hello world"


target("Hello world").

test(levenshtein) :-
    Distance = 0,
    Input = "Hello world",
    target(Target),
    levenshtein(Input, Target, Distance).

test(levenshtein) :-
    Distance = 1,
    Input = "Hello worl",
    target(Target),
    levenshtein(Input, Target, Distance).

test(levenshtein) :-
    Distance = 1,
    Input = "xHello world",
    target(Target),
    levenshtein(Input, Target, Distance).

test(levenshtein) :-
    Distance = 11,
    Input = "",
    target(Target),
    levenshtein(Input, Target, Distance).


test(mapcost) :-
    mapcost("accuracy", ["Hello","Hell"], [0,1]).

test(stopcondition) :-
    stopcondition("zero_cost", "levenshtein", ["Hello"]).

:- end_tests(tasks).
