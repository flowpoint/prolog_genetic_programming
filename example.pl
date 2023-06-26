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
    Initializer = [["pred_fun(X,Y) :- Y is X."]],
    true
)
).


:- dynamic pred_fun/2.

evalu(Str, Inp, Res) :-
    term_string(T, Str),
    retractall(pred_fun),
    asserta(T),
    apply(pred_fun, [Inp, Res]),
    !.

evalua(Str, Inp, Res) :-
    string_concat("pred_fun(X,Y) :- Y is", _, Str),evalu(Str, Inp, Res);
    Res=200.0.


eval_catch(Gene, Inp, Pred) :-
    catch(evalua(Gene, Inp, P), Error, P=100.0),
    Pred = P,
    !.

% eval_catch("pred_fun(A,B):- B is A+1., 1, P).
:- retractall(tasks:costfn).
:- asserta(
(
tasks:costfn("distance_from_fun", Gene, Cost) :- 
    %random(0.0, 100.0, Input),
    Input=1.0,
    %number_string(Input, Gene),
    target_fun(Input, Res),
    eval_catch(Gene, Input, Pred),
    Cost is abs(Res-Pred)
)
).


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


%:- genetic_programming:genetic_programming("Learn_polynomial_function", "stringopt", B).
:- genetic_programming:genetic_programming("Learn_polynomial_function", "termopt", B).
