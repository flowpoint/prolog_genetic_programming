% ========================================================================================
% Lecture/Project : LOGIC PROGRAMMING - Genetic Programming
% ----------------------------------------------------------------------------------------
% E Shin
% ========================================================================================

% ########################################################################################
% Version: 1.0
% ########################################################################################


% ========================================================================================

% C:/Programme/swipl/bin/swipl   

% ========================================================================================

% Generate a random gene
generate_random_gene(Target, gene(Code, Mutate_Chance, Cost_Value)) :-
    % Code1: A random number between 33-126
    random_between(33, 126, Code1),
    % Mutate_Chance: A random value between 1 - 10
    random_between(1, 10, Mutate_Chance),
    % Char1: Decoding the decimal from Code1 to an ASCII
    char_code(Char1, Code1),
    random_between(33, 126, Code2),
    char_code(Char2, Code2),
    random_between(33, 126, Code3),
    char_code(Char3, Code3),
    random_between(33, 126, Code4),
    char_code(Char4, Code4),
    % Temp1: Concat Char1 and Char2 as Temp1.
    atom_concat(Char1, Char2, Temp1),
    atom_concat(Temp1, Char3, Temp2),
    % Code: Is a random concatenation of 4 random numbers between 33-126.
    atom_concat(Temp2, Char4, Code),
    % Calculate the cost for the generated gene
    calculate_target_cost(Code, Target, Cost_Value).

% Helper predicate to generate a list of random genes
% Base case: Empty list
generate_genes_list_helper(_, 0, []).
% Recursive case: Generate a random gene and append it to the list
generate_genes_list_helper(Target, N, [Gene|Rest]) :-
    N > 0,
    generate_random_gene(Target, Gene),
    N1 is N - 1,
    generate_genes_list_helper(Target, N1, Rest).

% ========================================================================================

% Calculate the cost for a gene based on the target string
calculate_target_cost(Code, Target, Cost) :-
    string_chars(Target, TargetChars),
    string_chars(Code, CodeChars),
    calculate_cost_helper(CodeChars, TargetChars, 0, Cost).

% Base case: Empty lists
calculate_cost_helper([], [], Acc, Acc).
% Base case: Code is empty
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
    Cost1 is sqrt(ASCII1**2 + ASCII2**2),
    NewAcc is Acc + Cost1,
    calculate_cost_helper(CodeRest, TargetRest, NewAcc, Cost).

% ========================================================================================

% Sort a list of genes by their cost
sort_genes_list(Genes, SortedGenes) :-
    predsort(compare_genes, Genes, SortedGenes).

% Comparison predicate for sorting genes based on their cost
compare_genes(Order, gene(_, _, Cost1), gene(_, _, Cost2)) :-
    compare(Order, Cost1, Cost2).

% ========================================================================================

% Main predicate to generate a list of random genes and sort them by their cost
% generate_genes_list("Hello", Genes).
generate_genes_list(Target, Size, SortedGenes) :-
    generate_genes_list_helper(Target, Size, Genes),
    sort_genes_list(Genes, SortedGenes).