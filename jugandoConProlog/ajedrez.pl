:- use_module(utils).
/* 
	 ___ ___ ___ ____ ____ ___ ___ ___ 
	|___|___|___|____|____|___|___|___|
	|___|___|___|____|____|___|_t_|___|
	|___|___|___|____|____|___|___|___|
	|___|___|___|____|__a_|___|___|___|
	|___|___|___|____|____|___|___|___|
	|___|___|___|____|____|___|___|___|
	|_P_|_P_|_P_|_P__|_P__|_P_|_P_|_P_|
  1	|_T_|_C_|_A_|_RA_|_RY_|_A_|_C_|_T_|
	1
*/

hijo(homero, bart).
hijo(homero, lisa).
hijo(homero, maggie).
hijo(marge, bart).
hijo(marge, lisa).
hijo(marge, maggie).
hijo(ned, tod).
hijo(ned, rod).

pieza(torre, pos(blanco, 1, 1)).
pieza(caballo, pos(blanco, 2, 1)).
pieza(alfil, pos(blanco, 3, 1)).
pieza(reina, pos(blanco, 4, 1)).
pieza(rey, pos(blanco, 5, 1)).
pieza(alfil, pos(blanco, 6, 1)).
pieza(caballo, pos(blanco, 7, 1)).
pieza(torre, pos(blanco, 8, 1)).
pieza(peon, pos(blanco, 1, 2)).
pieza(peon, pos(blanco, 2, 2)).
pieza(peon, pos(blanco, 3, 2)).
pieza(peon, pos(blanco, 4, 2)).
pieza(peon, pos(blanco, 5, 2)).
pieza(peon, pos(blanco, 6, 2)).
pieza(peon, pos(blanco, 7, 2)).
pieza(peon, pos(blanco, 8, 2)).

pieza(torre, pos(negro, 2, 2)).
pieza(alfil, pos(negro, 4, 4)).

enemigo(blanco, negro).
enemigo(negro, blanco).

posicion(pieza(_, Pos), Pos).
color(pos(Color, _, _), Color).
coordX(pos(_, X, _), X).
coordY(pos(_, _, Y), Y).

piezaEn(Posicion, Pieza) :- 
	posValida(Posicion),
	aliadoEn(Posicion, Pieza).
piezaEn(Posicion, Pieza) :- 
	posValida(Posicion),
	enemigoEn(Posicion, Pieza).

enemigoEn(pos(Color, X, Y), Enemigo) :-
	enemigo(Color, ColorEnemigo),
	pieza(T, pos(ColorEnemigo, XEnemigo, YEnemigo)),
	X is 9 - XEnemigo,
	Y is 9 - YEnemigo,
	Enemigo = pieza(T, pos(Color, X, Y)).

aliadoEn(Posicion, Pieza) :-
	pieza(T, Posicion),
	Pieza = pieza(T, Posicion).
	
posValida(pos(Color, X, Y)) :-
	colorValido(Color),
	between(1, 8, X),
	between(1, 8, Y).
	
colorValido(negro).
colorValido(blanco).
	
%%% %MOVIMIENTOS
movimiento(Pieza, Posicion) :-
	posValida(Posicion),
	posibleMov(Pieza, Posicion),
	not(posicion(Pieza, Posicion)).

posibleMov(pieza(torre, PosActual), PosFutura) :-
	movimientoTorre(PosActual, PosFutura),
	forall(posEntre(PosActual, PosFutura, PosMedio),
		not(piezaEn(PosMedio, _))).

posibleMov(pieza(caballo, PosActual), PosFutura) :-
	movimientoCaballo(PosActual, PosFutura),
	not(aliadoEn(PosFutura, _)).

posibleMov(pieza(alfil, PosActual), PosFutura) :-
	movimientoAlfil(PosActual, PosFutura),
	forall((posEntre(PosActual, PosFutura, PosMedio), movimientoAlfil(PosActual, PosMedio)),
		not(piezaEn(PosMedio, _))).
	
posibleMov(pieza(rey, PosActual), PosFutura) :-
	movimientoRey(PosActual, PosFutura),
	not(aliadoEn(PosFutura, _)).
		
posibleMov(pieza(peon, PosActual), PosFutura) :-
	movimientoPeon(PosActual, PosFutura),
	not(aliadoEn(PosFutura, _)).

	
%Peon	
movimientoPeon(pos(Color, X, 2), pos(Color, X, 4)).
movimientoPeon(pos(Color, X, Y), pos(Color, X, PY)) :-
	PY is Y + 1.
movimientoPeon(pos(Color, X, Y), Posicion) :-
	PX is X + 1,
	PY is Y + 1,
	Posicion = pos(Color, PX, PY),
	enemigoEn(Posicion, _).

%Caballo
movimientoCaballo(pos(Color, X, Y), pos(Color, PX, PY)) :-
	PX is X + 2,
	PY is Y + 1.
movimientoCaballo(pos(Color, X, Y), pos(Color, PX, PY)) :-
	PX is X + 2,
	PY is Y - 1.	
movimientoCaballo(pos(Color, X, Y), pos(Color, PX, PY)) :-
	PX is X - 2,
	PY is Y + 1.
movimientoCaballo(pos(Color, X, Y), pos(Color, PX, PY)) :-
	PX is X - 2,
	PY is Y - 1.
movimientoCaballo(pos(Color, X, Y), pos(Color, PX, PY)) :-
	PX is X + 1,
	PY is Y + 2.
movimientoCaballo(pos(Color, X, Y), pos(Color, PX, PY)) :-
	PX is X + 1,
	PY is Y - 2.
movimientoCaballo(pos(Color, X, Y), pos(Color, PX, PY)) :-
	PX is X - 1,
	PY is Y + 2.
movimientoCaballo(pos(Color, X, Y), pos(Color, PX, PY)) :-
	PX is X - 1,
	PY is Y - 2.
	
%Rey
movimientoRey(pos(Color, X, Y), pos(Color, PX, Y)) :-
	PX is X + 1.
movimientoRey(pos(Color, X, Y), pos(Color, PX, Y)) :-
	PX is X - 1.
movimientoRey(pos(Color, X, Y), pos(Color, X, PY)) :-
	PY is Y + 1.
movimientoRey(pos(Color, X, Y), pos(Color, X, PY)) :-
	PY is Y - 1.
	
%Torre
movimientoTorre(pos(Color, X, _), pos(Color, X, _)).
movimientoTorre(pos(Color, _, Y), pos(Color, _, Y)).

%Alfil
movimientoAlfil(pos(Color, X, Y), pos(Color, DX, DY)) :-
	comp(mod,subs(X), DX, Dist),
	comp(mod,subs(Y), DY, Dist).
%%%
	
posEntre(pos(Color, X1, Y1), pos(Color, X2, Y2), pos(Color, X, Y)) :-
	between(X1, X2, X),
	between(Y1, Y2, Y).
posEntre(pos(Color, X1, Y1), pos(Color, X2, Y2), pos(Color, X, Y)) :-
	between(X1, X2, X),
	between(Y2, Y1, Y).
posEntre(pos(Color, X1, Y1), pos(Color, X2, Y2), pos(Color, X, Y)) :-
	between(X2, X1, X),
	between(Y1, Y2, Y).
posEntre(pos(Color, X1, Y1), pos(Color, X2, Y2), pos(Color, X, Y)) :-
	between(X2, X1, X),
	between(Y2, Y1, Y).
	

	