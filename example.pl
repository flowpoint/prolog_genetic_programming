:- use_module(src/tasks).
:- use_module(src/genetic_programming).
:- use_module(src/optimizer).

% adding tasks is easy by dynamically adding a task definition here

target_fun(X, Y) :-
    Y is X*X+1.0.

:- retractall(tasks:task).
:- asserta(
(
tasks:task("Learn_polynomial_function", "distance_from_fun", Initializer, "zero_cost") :-
    Initializer = [[
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X.",
    "pred_fun(X,Y) :- Y is X."
    ]],
    true
)
).


:- dynamic pred_fun/2.

% run term body
evalu(Str, Inp, Res) :-
    term_string(T, Str),
    retractall(pred_fun),
    asserta(T),
    apply(pred_fun, [Inp, Res]),
    !.

% ensure the term head is correct
evalua(Str, Inp, Res) :-
    string_concat("pred_fun(X,Y) :- Y is", _, Str),evalu(Str, Inp, Res);
    Res=200.0.

% catch incorrect terms, but assume the term head is correct
eval_catch(Gene, Inp, Pred) :-
    catch(evalua(Gene, Inp, P), Error, P=100.0),
    Pred = P,
    !.

% add a new cost function
:- retractall(tasks:costfn).
:- asserta(
(
tasks:costfn("distance_from_fun", Gene, Cost) :- 
    random(0.0, 10.0, Input),
    target_fun(Input, Res),
    eval_catch(Gene, Input, Pred),
    Cost is abs(Res-Pred)
)
).

% change the gene base symbols
:- retractall(optimizer:symbols).
:- asserta(
(
symbols(L) :-
    string_chars(
    %"pred_fun()X,Y:-is *+0123456789.",
    "X *+0123456789.",
    L)
)
).

% example of a overridden optimizer configuration
:- asserta(
(
optimizer:optimizer("termopt", Selectionop, Crossoverop, Mutationop) :-
    Selectionop="top1k",
    Crossoverop="headtail",
    Mutationop="indel",
    selection(Selectionop, _, _, _),
    crossover(Crossoverop, _, _),
    mutate(Mutationop, _, _),
    !
)
).

% only mutate the latter part of the term
:- retractall(optimizer:mutate_gene).
:- asserta(
(
mutate_gene(Gene, NewGene) :-
    gene(Gene),
    string_length(Gene, L),
    K is L-1,
    random_between(22, K, Index),
    random_member(Indel, ["insertion","deletion","replacement"]),
    mutate_pos(Indel, Index, Gene, NewGene),
    !
)
).

:- genetic_programming:genetic_programming("Learn_polynomial_function", "termopt", B).
