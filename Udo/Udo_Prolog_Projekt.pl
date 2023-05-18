% ========================================================================================
% Lecture/Project : LOGIC PROGRAMMING - Genetic Programming
% ----------------------------------------------------------------------------------------
% E Shin
% ========================================================================================

% ########################################################################################
% Version: 1.0
% ########################################################################################

% Define the constructor predicate
new_person(Name, Age, Person) :-
    Person = person(Name, Age).

% Getter to unify result to Name
get_person_name(person(Name, _), Name).


new_gene(String, Cost, Gene) :-
    Gene = new_gene(String,Cost).

get_gene(person(Name, _), Name).
