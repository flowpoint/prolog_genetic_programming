:- begin_tests(cost_function).
:- use_module(core).
:- use_module(cost_function).

%------------------------------------------------------------------------------------------------------------------------
% Tests for the cost function
% Target: "Hello world"
target("Hello world").

%------------------------------------------------------------------------------------------------------------------------
% Test levenshtein cost function
test(levenshtein_1) :-
    Distance = 0,
    Input = "Hello world",
    target(Target),
    levenshtein(Input, Target, Distance).

test(levenshtein_2) :-
    Distance = 1,
    Input = "Hello worl",
    target(Target),
    levenshtein(Input, Target, Distance).

test(levenshtein_3) :-
    Distance = 1,
    Input = "xHello world",
    target(Target),
    levenshtein(Input, Target, Distance).

test(levenshtein_4) :-
    Distance = 11,
    Input = "",
    target(Target),
    levenshtein(Input, Target, Distance).

%------------------------------------------------------------------------------------------------------------------------
% Test quadratic cost function
test(quadratic_cost_1) :-
    Input = "Hello world",
    target(Target),
    Cost = 0.0,
    quadratic_cost(Input, Target, Cost).

test(quadratic_cost_2) :-
    Input = "Hello worl",
    target(Target),
    Cost = 2000.0,
    quadratic_cost(Input, Target, Cost).

test(quadratic_cost_3) :-
    Input = "Hello worldx",
    target(Target),
    Cost = 2000.0,
    quadratic_cost(Input, Target, Cost).

% Test: Calculation of the cost
% ASCII Value: A = 65, B = 66, C = 67
% Cost = sqr(|66^2 - 65r^2|) + 0 + 0 = 11.445523142259598
test(quadratic_cost_4) :-
    Input = "ABC",
    Target = "BBC",
    Cost = 11.445523142259598,
    quadratic_cost(Input, Target, Cost).

%------------------------------------------------------------------------------------------------------------------------
% Test mapcost
test(mapcost_1) :-
    mapcost("levenshtein", ["Hello","Hell"], [0,1]).

test(mapcost_2) :-
    mapcost("quadratic_cost", ["Hello","Hell"], [0.0,2000.0]).

%------------------------------------------------------------------------------------------------------------------------
% Test stopcondition
test(stopcondition_1) :-
    stopcondition("zero_cost", "levenshtein", ["Hello"]).

test(stopcondition_2) :-
    stopcondition("zero_cost", "quadratic_cost", ["Hello"]).

:- end_tests(tasks).
