% ========================================================================================
% Lecture/Project : LOGIC PROGRAMMING - Genetic Programming
% ----------------------------------------------------------------------------------------
% E Shin
% ========================================================================================

% ########################################################################################
% Version: 1.0
% ########################################################################################

% Gene constructor
% new_gene(Code, Cost_Value, Gene, gene(Code, Cost_Value)).

% Generate a random gene
% Gene has 2 attributes: Code: 4 ASCII Values | Cost_Value: init 9999
generate_random_gene(gene(Code, 9999)) :-
    % Code1: A random number between 33-126
    random_between(33, 126, Code1),  
    % Char1: Decoding the decimal from Code1 to an ASCCI 
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
    % Code: Is a random concat of 4 random numbers between 33-126.
    atom_concat(Temp2, Char4, Code).

% Generate a list of random genes
% generate_random_genes(100,Genes).
generate_random_genes(0, []).
generate_random_genes(N, [Gene|Rest]) :-
    N > 0,
    generate_random_gene(Gene),
    N1 is N - 1,
    generate_random_genes(N1, Rest).

% Findall to find all genes, which a specific cost
% get_gene_by_cost(9999, Genes).
get_gene_by_cost(Cost_Value, Genes) :-
    findall(gene(Code, Cost_Value), new_gene(Code, Cost_Value, _), Genes).
