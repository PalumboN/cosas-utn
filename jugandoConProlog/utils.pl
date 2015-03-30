:- module(utils, 
	[bigger/2,
	smaller/2,
	sum/3,
	subs/3,
	mod/2,
	filter/3,
	map/3,
	findAll/2,
	first/2,
	first/3,
	single/3,
	any/2,
	all/2,
	empty/1,
	flip/3,
	comp/4,
	countAll/2,
	count/3,
	max/2,
	min/2,
	equals/2,
	notEquals/2,
	getPredicate/2,
	intoBetween/3]).

bigger(X, Y) :- X > Y.
smaller(X, Y) :- X < Y.
	
sum(X, Y, R) :- R is X + Y.	
subs(X, Y, R) :- R is X - Y.
	
mod(X, Y) :- Y is abs(X).
	
filter(Cond, Lista, Filtrados) :-
	findall(Elem,
	(member(Elem, Lista), call(Cond, Elem)),
	Filtrados).
	
map(Trans, Lista, Transformados) :-
	findall(T,
	(member(Elem, Lista), call(Trans, Elem, T)),
	Transformados).

findAll(Cond, Lista) :-
	catch(findall(Elem, call(Cond, Elem), Lista),
	error(existence_error(_, _), _),
	(X = call(Cond, _), findAll(X, Lista))).
	
first([X|_], X).
first(Cond, Lista, Primero) :-
	filter(Cond, Lista, [Primero|_]).

single(Cond, Lista, Unico) :-
	filter(Cond, Lista, [Unico|Xs]),
	length(Xs, 0).
	
any(Cond, Lista) :-
	member(Elem, Lista),
	call(Cond, Elem).

all(Cond, Lista) :-
	not(empty(Lista)),
	forall(member(Elem, Lista), call(Cond, Elem)).
	
empty([]).

flip(Cond, X, Y) :- call(Cond, Y, X).

comp(SegundoPred, PrimerPred, X, R) :-
	call(PrimerPred, X, Y),
	call(SegundoPred, Y, R).

countAll(Cond, Cant) :-
	comp(length, findAll, Cond, Cant).
	
count(Cond, Lista, Cant) :-
	comp(length, filter(Cond), Lista, Cant).
	
max(List, Max) :-
	member(Max, List),
	forall(member(Elem, List), Elem =< Max).
	
min(List, Min) :-
	member(Min, List),
	forall(member(Elem, List), Elem >= Min).
	
getPredicate(Cond, Predicate) :-
	catch((
	Cond =.. Params, 
	append(Params,[X], Full), 
	PosiblePredicate =.. Full,
	PosiblePredicate,
	PosiblePredicate =.. PosibleParams,
	getRealParams(PosibleParams, RealParams),
	Predicate =.. RealParams
	),
	error(existence_error(_, _), _),
	(
	New = call(Cond, X),
	getPredicate(New, Predicate)
	)).
	
getRealParams(PosibleParams, RealParams) :-
	filter(notEquals(call), PosibleParams, Params),
	checkFlip(Params, RealParams).
	
checkFlip([flip, X, Y, Z | Ps], [X, Z, Y | RPs]) :-
	checkFlip(Ps, RPs), !.
checkFlip([X | Ps], [X | RPs]) :-
	checkFlip(Ps, RPs), !.
checkFlip([], []).
	
equals(X, X).
notEquals(X, Y) :- X \= Y.

intoBetween(X, Y, R) :-
	succ(X, X1),
	succ(Y1, Y),
	between(X1, Y1, R).