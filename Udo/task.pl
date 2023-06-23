:- module(task, [task/4]).
:- use_module(core).
:- use_module(cost_function)

%----------------------------------------------------------------------------------------------
% Task 1
task(
    "Learn_String_with_Levenshtein", 
    "levenshtein",
    [[
    "Levenshtein Go",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
    ]],
    "zero_cost"
    ) :-
        true.

%----------------------------------------------------------------------------------------------
% Task 2
task(
    "Learn_String_with_Quadratic", 
    "quadratic_cost",
    [[
    "Quadratic Go",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
    ]],
    "zero_cost"
    ) :-
        true.

%----------------------------------------------------------------------------------------------
% Task 3
task("Learn_String_with_Population_Generator", "quadratic_cost", Initializer, "zero_cost") :-
    generate_population(10, 10, Population),
    Initializer = [Population],
    true.