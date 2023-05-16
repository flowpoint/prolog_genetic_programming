:- module(genetic_programming, [genetic_programming/3]).

% taskspec is some structure that contains:
% initialization/initial population
% selectionop
% crossoverop
% mutationop
% stopcondition
% costfunction

task([TaskName, Costfn, Initializer, StopCondition]) :-
    TaskName = "Learn String",
    Costfn = _,
    Initializer = _,
    StopCondition = _.

%optimizer(Selectionop, Crossoverop, Mutationop).
optimizer(_,_,_).

 genetic_programming(Task, Optimizer, EvolutionHistory) :-
     task(Task),
     Optimizer = _,
     EvolutionHistory = ["Hello world"].

    
