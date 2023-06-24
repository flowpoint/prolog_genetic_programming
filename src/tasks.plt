:- begin_tests(tasks).
:- use_module(core).
:- use_module(tasks).

%-----------------------------------------------------------------------------------------------------------------------
% Tests for the tasks module
target("Hello world").

%-----------------------------------------------------------------------------------------------------------------------
% Test the levenshtein distance
% Test: Input and Target are the same -> Distance = 0
test(levenshtein_1) :-
    Distance = 0,
    Input = "Hello world",
    target(Target),
    levenshtein(Input, Target, Distance).
% Test: Input and Target are different by 1 missing character at the end -> Distance = 1
test(levenshtein_2) :-
    Distance = 1,
    Input = "Hello worl",
    target(Target),
    levenshtein(Input, Target, Distance).
% Test: Input and Target are different by 1 extra character at the end -> Distance = 1
test(levenshtein_3) :-
    Distance = 1,
    Input = "xHello world",
    target(Target),
    levenshtein(Input, Target, Distance).
% Test: Input is empty -> Distance = 11
test(levenshtein_4) :-
    Distance = 11,
    Input = "",
    target(Target),
    levenshtein(Input, Target, Distance).

%-----------------------------------------------------------------------------------------------------------------------
% Test the stopcondition
test(stopcondition) :-
    stopcondition("zero_cost", "levenshtein", ["Hello"]).

%-----------------------------------------------------------------------------------------------------------------------
% Test quadratic cost function
% Test: Input and Target are the same -> Cost = 0
test(quadratic_cost_1) :-
    Input = "Hello world",
    target(Target),
    Cost = 0.0,
    quadratic_cost(Input, Target, Cost).
% Test: Input and Target are different by 1 missing character at the end -> Cost = 2000
test(quadratic_cost_2) :-
    Input = "Hello worl",
    target(Target),
    Cost = 2000.0,
    quadratic_cost(Input, Target, Cost).
% Test: Input and Target are different by 1 extra character at the end -> Cost = 2000
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

:- end_tests(tasks).
