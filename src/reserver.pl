% Generate a list with random genes.
% Parameter: Size, Char_Limit, TargetList
generate_population(Size, Char_Limit, Population) :-
    generate_genes_list_helper(Size, Char_Limit, Population).

generate_genes_list_helper(0, _, []).

generate_genes_list_helper(Size, Char_Limit, [Gene | Rest]) :-
    Size > 0,
    generate_random_gene(Char_Limit, Gene),
    Size1 is Size - 1,
    generate_genes_list_helper(Size1, Char_Limit, Rest).

generate_random_gene(Char_Limit, Gene) :-
    random_between(1, Char_Limit, Length),
    generate_random_code(Length, Gene).

generate_random_code(Length, Gene) :-
    generate_random_code_helper(Length, '', Gene).

generate_random_code_helper(0, Gene, Gene).
generate_random_code_helper(Length, Acc, Gene) :-
    Length > 0,
    random_between(33, 126, Code),
    char_code(Char, Code),
    atom_concat(Acc, Char, Acc1),
    Length1 is Length - 1,
    generate_random_code_helper(Length1, Acc1, Gene).


% Rework in Progress
quadratic_cost(Input, Target, Cost) :-
    string_chars(Target, TargetChars),
    string_chars(Input, InputChars),
    calculate_cost_helper(InputChars, TargetChars, 0, Cost).

% Base case: Input and Target is empty
calculate_cost_helper([], [], Acc, Acc).
% Base case: Target is empty
calculate_cost_helper(Code, [], Acc, Cost) :-
    length(Code, CodeLength),
    Cost is Acc + (2000 * CodeLength).
% Base case: Code is empty
calculate_cost_helper([], Target, Acc, Cost) :-
    length(Target, TargetLength),
    Cost is Acc + (2000 * TargetLength).
% Calculate the cost for each pair of characters
calculate_cost_helper([C1 | CodeRest], [T1 | TargetRest], Acc, Cost) :-
    char_code(C1, ASCII1),
    char_code(T1, ASCII2),
    Cost_tmp is sqrt(abs((ASCII2**2) - (ASCII1**2))),
    NewAcc is Acc + Cost_tmp,
    calculate_cost_helper(CodeRest, TargetRest, NewAcc, Cost).

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