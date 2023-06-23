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

test(stopcondition) :-
    stopcondition("zero_cost", "levenshtein", ["Hello"]).

% Test quadratic cost function
% Test: Input and Target are the same -> Cost = 0
test(quadratic_cost) :-
    Input = "Hello world",
    target(Target),
    Cost = 0.0,
    quadratic_cost(Input, Target, Cost).


% Test: Input and Target are different by 1 missing character at the end -> Cost = 2000
test(quadratic_cost) :-
    Input = "Hello worl",
    target(Target),
    Cost = 2000.0,
    quadratic_cost(Input, Target, Cost).


% Test: Input and Target are different by 1 extra character at the end -> Cost = 2000
test(quadratic_cost) :-
    Input = "Hello worldx",
    target(Target),
    Cost = 2000.0,
    quadratic_cost(Input, Target, Cost).


% Test: Calculation of the cost
% ASCII Value: A = 65, B = 66, C = 67
% Cost = sqr(|66^2 - 65r^2|) + 0 + 0 = 11.445523142259598
test(quadratic_cost) :-
    Input = "ABC",
    Target = "BBC",
    Cost = 11.445523142259598,
    quadratic_cost(Input, Target, Cost).

:- end_tests(tasks).
