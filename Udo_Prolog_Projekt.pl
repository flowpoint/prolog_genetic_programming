% ========================================================================================
% Lecture/Project : LOGIC PROGRAMMING - Genetic Programming
% ----------------------------------------------------------------------------------------
% E Shin
% ========================================================================================

% ########################################################################################
% Version: 1.0
% ########################################################################################

% Gene class

% This is the constructor method of the Gene class.
% It initializes a new instance of Gene with a code attribute and sets
% the cost attribute to 9999.
gene_init(Gene, Code) :-
    Gene = gene{code: Code, cost: 9999}.

% This method retrieves the code attribute of a Gene object.
gene_code(Gene, Code) :-
    Gene = gene{code: Code, _}.

% This method takes another Gene object as input and performs mating or
% crossover operation between the current gene and the input gene. It
% splits the code of both genes at the middle index and creates two new
% genes by combining the first half of the code from one gene with the
% second half of the code from the other gene, and vice versa. It returns
% a list containing the two new genes created.
gene_mate(Gene1, Gene2, NewGenes) :-
    string_chars(Gene1.code, Gene1CodeList),
    string_chars(Gene2.code, Gene2CodeList),
    append(FirstHalfGene1CodeList, SecondHalfGene1CodeList, Gene1CodeList),
    append(FirstHalfGene2CodeList, SecondHalfGene2CodeList, Gene2CodeList),
    append(FirstHalfGene1CodeList, SecondHalfGene2CodeList, Code1),
    append(FirstHalfGene2CodeList, SecondHalfGene1CodeList, Code2),
    gene_init(Gene3, Code1),
    gene_init(Gene4, Code2),
    NewGenes = [Gene3, Gene4].

% This method introduces random mutations in the code of the gene based
% on a given chance value. If a randomly generated number is less than
% chance, the method performs the mutation. It selects a random index in
% the code and either increments or decrements the ASCII value of the
% character at that index by 1, with a restriction that the new ASCII
% value should be within the range of 1 to 255.
gene_mutate(Gene, Chance) :-
    random(X),
    (X < Chance ->
        true
    ;
        gene_code(Gene, Code),
        length(Code, Length),
        Index is round(X * Length),
        nth0(Index, Code, Char),
        random(0, 2, UpOrDown),
        (UpOrDown = 0 ->
            NewChar is Char - 1
        ;
            NewChar is Char + 1
        ),
        between(1, 255, NewChar),
        nth0(Index, NewCode, NewChar, Code),
        Gene = Gene.put(code, NewCode)
    ).

% This method generates a random code of a specified length by creating a
% string of characters with random ASCII values ranging from 0 to 255.
gene_random(Gene, Length) :-
    length(Code, Length),
    maplist(random_between(0, 255), Code),
    Gene = Gene.put(code, Code).

% This method calculates the cost of the gene by comparing its code with a
% given target string. It iterates over each character in the code and calculates
% the squared difference between the ASCII values of the character in the gene and
% the corresponding character in the target string. The sum of all squared differences
% is stored in the cost attribute of the gene.
gene_calc_cost(Gene, Target) :-
    gene_code(Gene, Code),
    string_chars(Code, CodeList),
    string_chars(Target, TargetList),
    maplist(compare_squared_diff, CodeList, TargetList, SquaredDiffList),
    sum_list(SquaredDiffList, Cost),
    Gene = Gene.put(cost, Cost).

% Helper predicate to compare the squared difference of two characters.
% Helper predicate to compare the squared difference of two characters.
compare_squared_diff(Char1, Char2, SquaredDiff) :-
    char_code(Char1, Code1),
    char_code(Char2, Code2),
    Diff is Code1 - Code2,
    SquaredDiff is Diff * Diff.

% Population class

% This is the constructor method of the Population class. It initializes a new
% instance of Population with the given target string, size (the number of members
% in the population), and log_costs (a boolean flag indicating whether to log the
% costs of the highest ranking member). It creates a list of Gene objects as the
% population members, where each gene has a random code of the same length as the
% target string. It also initializes the generationNumber attribute to 0.
population_init(Population, Target, Size, LogCosts) :-
    Population = population{target: Target, members: Members, generationNumber: 0, log_costs: LogCosts},
    findall(Gene, (between(1, Size, _), gene_random(Gene, Length)), Members).

% This method calculates the cost for each member in the population by calling
% the calcCost() method of each Gene object.
population_calc_costs(Population) :-
    population_members(Population, Members),
    population_target(Population, Target),
    maplist(gene_calc_cost(Target), Members).

% This method applies the mutate() method to each member in the population,
% which introduces random mutations in their codes based on the given chance value.
population_mutate(Population, Chance) :-
    population_members(Population, Members),
    maplist(gene_mutate(Chance), Members).

% This method sorts the population members based on their costs in ascending order.
% Should be called after Population.calcCosts()
population_sort(Population) :-
    population_members(Population, Members),
    predsort(compare_gene_costs, Members, SortedMembers),
    Population = Population.put(members, SortedMembers).

% Comparison predicate to sort genes based on their costs.
compare_gene_costs(Order, Gene1, Gene2) :-
    gene_cost(Gene1, Cost1),
    gene_cost(Gene2, Cost2),
    (Cost1 < Cost2 -> Order = '<' ; Order = '>').

% This method calculates the costs, sorts the population, and displays the generation
% number, the code of the member with the lowest cost (highest ranking member), and its cost.
population_display(Population) :-
    population_calc_costs(Population),
    population_sort(Population),
    population_generation_number(Population, GenerationNumber),
    format('Generation ~w ~s ~w~n', [GenerationNumber, Population.members[0].code, Population.members[0].cost]).

% This method performs a generation of the genetic algorithm. It repeatedly calls
% the _generation() method until a member in the population has the same code as
% the target string. If display is true, it displays the population details during
% each generation. If log_costs is true, it returns a list containing the costs of
% the highest ranking member at each generation; otherwise, it returns the total
% number of generations.
population_generation(Population, Display, LogCosts, Result) :-
    (population_generation_loop(Population, Display, LogCosts, Result) ->
        true
    ;
        population_generation(Population, Display, LogCosts, Result)
    ).

population_generation_loop(Population, Display, LogCosts, Result) :-
    population_generation_display(Population, Display),
    (LogCosts ->
        population_cost_log_append(Population)
    ;
        true
    ),
    population_members(Population, Members),
    Members = [Gene1, Gene2 | _],
    gene_mate(Gene1, Gene2, NewGenes),
    NewMembers = [NewGene1, NewGene2 | RestMembers],
    append(NewMembers, [NewGene1, NewGene2 | RestMembers], UpdatedMembers),
    population_replace_members(Population, UpdatedMembers),
    population_mutate(Population, 0.5),
    population_calc_costs(Population),
    (population_check_target_match(Population) ->
        population_sort(Population),
        population_generation_display(Population, Display),
        Result = true
    ;
        population_increment_generation_number(Population),
        Result = false
    ),
    population_display(Population). % Add this line to display the population details.


% This method performs a single generation of the genetic algorithm. It calculates costs,
% sorts the population, logs the cost if log_costs is true, displays the population if
% display is true, performs mating between the two highest ranking members, replaces the
% two lowest ranking members with the children generated from mating, mutates all members
% (except the highest ranking member), recalculates their costs, checks if any member has
% the same code as the target string, and updates the generation number.
population_display(Population) :-
    population_calc_costs(Population),
    population_sort(Population),
    (Display ->
        population_display(Population)
    ;
        true
    ).

% Helper predicates to access population attributes.
population_target(Population, Target) :-
    Population = population{target: Target}.
population_members(Population, Members) :-
    Population = population{members: Members}.
population_generation_number(Population, GenerationNumber) :-
    Population = population{generationNumber: GenerationNumber}.
population_log_costs(Population, LogCosts) :-
    Population = population{log_costs: LogCosts}.
population_cost_log(Population, CostLog) :-
    Population = population{cost_log: CostLog}.

% Helper predicates to modify population attributes.
population_increment_generation_number(Population) :-
    population_generation_number(Population, GenerationNumber),
    NewGenerationNumber is GenerationNumber + 1,
    Population = Population.put(generationNumber, NewGenerationNumber).
population_replace_members(Population, NewMembers) :-
    Population = Population.put(members, NewMembers).
population_cost_log_append(Population) :-
    population_cost_log(Population, CostLog),
    population_members(Population, Members),
    Members = [Gene | _],
    gene_cost(Gene, Cost),
    append(CostLog, [Cost], NewCostLog),
    Population = Population.put(cost_log, NewCostLog).

% Main predicate
main :-
    gene_init(Gene1, "AAAAAAAAAAAAAA"),
    gene_init(Gene2, "AAAAAAAAAAAAAA"),
    gene_mate(Gene1, Gene2, Children),
    member(Child, Children),
    gene_calc_cost(Child, "Hi, mein Name ist Udo!"),
    format('~s ~w~n', [Child.code, Child.cost]),

    population_init(Population, "Hi, mein Name ist Udo!", 100, true),
    population_generation(Population, true, true, CostLog),
    writeln(CostLog).