% ========================================================================================
% Lecture/Project : LOGIC PROGRAMMING
% ----------------------------------------------------------------------------------------
% M Mueller
% ========================================================================================

% ########################################################################################
% 01
% ########################################################################################

% Prolog: PRO gramming in LOG ic
%         - Denotational Demantics = Horn Logic
%         - Procedural Semantics   = SLDNF, WAM


% ----------------------------------------------------------------------------------------
% Horn clauses

% Unary Horn clauses { fact } with exactly one positive and no negative literals are
% written as:
%
% {fact} 


fact( this_is_a_fact ).

body_1( _ThisIsAAvariable, just_an_atom ).

is_a_number( 7 ).

% Horn clauses with one positive ("head") and at least one negative literal ("body
% literals") are called rules:
%
% {head, ~body_1, ~body_2, ..., ~body_n}.

head( ARGUMENT , RESULT) :-
    body_1( ARGUMENT, BGUMENT),
    body_n( BGUMENT , RESULT).

body_n(_AnotherVariable_7, _Disregard).


% Horn clauses without a positive literal but at least on negative are called goal
% clauses:
%
% {~goal_1, ~goal_2, ..., ~goal_n}

:- fact( VARIABLE ), head(VARIABLE, _Answer).

% The empty clause always fails:

% fail.

% ----------------------------------------------------------------------------------------
% Terms and Unification

this_is_a_term(null).

this_is_a_term(s(X)) :-
    this_is_a_term(X).



myinteger(null).

myinteger(s(X)) :-
    myinteger(X).

myplus(X, arek, X).

myplus(X, s(Y), s(Z)) :-
    myplus(X, Y, Z).


kevin(null, 0).

kevin(s(X), YPE) :-
    kevin(X, Y),
    YPE is Y+1.

jam(_X, 0, 0).

jam(X, 1, X).

jam(X, Y, Z) :-
    YME is Y - 1,
    jam(X, YME , LZ),
    Z is X + LZ.

element(X, List) :-
    List = [X |_].

element(X, [_| R]) :-
    element(X, R).

% ########################################################################################
% 02
% ########################################################################################

% A term T appears in any list starting with  T

appearsin(T, [T|_]) :- !.

% If a term T appears in a list L, then it also is in the list L with some X prependended

appearsin(T, [_|L]) :-
    appearsin(T, L).

% When appending nothing to a list L, L remains unchanged.

concatlist([], L, L).

% If a list L with a list M together are a list N, then any term T prepending L also
% needs to prepend N

concatlist([X|L], M, [X|N]) :-
    concatlist(L, M, N).

% ----------------------------------------------------------------------------------------

data( adress( martin,
	      guy,
	      'n/a',
	      forty_foot_ln,
	      23,
	      dn39,
	      kirmington,
	      uk)
    ).



data( adress( potter,
	      harry,
	      harry,
	      eulenweg,
	      gryffindor_house,
	      cpo2lr,
	      hogwarts,
	      uk)
    ).
 
data( adress( clarkson,
	      jeremy,
	      diddly_squat,
	      chipping_norton_road,
	      '5-12',
	      'OX7 3PE',
	      chipping_norton,
	      uk)
    ).


data( income( clarkson, 820000 )).
data( income( martin  , 275000 )).
data( income( hamilton, 5000000)).
data( income( potter  , 20000)).

% What are the postcodes of people with income greater 25,000 ?


stock( think_pad, [ram-12, ssd-1024, cpu-i7, screen-11, brand-lenovo]).
stock( tecra,     [hdd-0.25, screen-8, cpu-pentium2, ram-0.4, brandf-toshiba]).
stock( macbook,   [cpu-a2, screen-12, brand-apple, ssd-2048, ram-64]).
stock( macbook,   [cpu-i5, brand-apple, screen-11, ssd-512, ram-16]).
stock( st,        [cpu-6800, brand-atari, screen-12, ram-1, fdd-1.4]).
stock( kaypro,    [cpu-8088, screen-3, fdd-320, fdd-320, ram-640]).
stock( appleII,   [cpu-68000, screen-12, brand-apple, fdd-180, ram-64]).

validattributenames([ram,ssd,cpu,screen,brand,hdd,fdd]).

processor( intel,  [8080, 8088, pentium, pentium2, i3, i5, i7]).
processor( amd,    [ryzen]).
processor( apple,  [a2, m1, a3, a4, m2]).
processor(motorola,[68000, 68020]).
processor(zyxel,   [z80]).



% show all 
% Who's got the smallest file storage?
% ...

get_street(Name, S) :-
    data(adress(Name,_,_,S,_,_,_,_)).


% countifs(+member, +liste, ?anzahl)



% ========================================================================================
% I/O

% see lib.


countifs( _X, [], 0) :- !.

countifs( X, [X|L], M) :-
    !,
    countifs( X, L, N),
    M is N +1.

countifs( X, [_|L], N) :-
    countifs( X, L, N).
