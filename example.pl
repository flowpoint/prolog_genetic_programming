:- use_module(src/tasks).
:- use_module(src/genetic_programming).

% adding tasks is easy by dynamically adding a task definition here

target_fun(x, y) :-
    y = x+1.

:- asserta(
(
tasks:task("Learn_polynomial_function", "quadratic_cost", Initializer, "zero_cost") :-
    Initializer = [[""]],
    true
)
).
