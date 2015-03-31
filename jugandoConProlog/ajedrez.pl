:- module(ajedrez).
:- use_module(utils).
/* 
	 ___8___7___6____5____4___3___2___1 
  8	|___|___|___|_ry_|____|___|___|___| 1
  7	|___|___|___|____|____|___|_t_|___| 2
  6	|___|___|___|____|____|___|___|___| 3
  5	|___|___|___|____|__a_|___|___|___| 4
  4	|___|___|___|_ra_|____|___|___|___| 5
  3	|_p_|___|_p_|____|____|___|___|___| 6
  2	|_P_|_P_|_P_|_P__|_P__|_P_|_P_|_P_| 7
  1	|_T_|_C_|_A_|_RA_|_RY_|_A_|_C_|_T_| 8
	1	 2	 3	 4	  5	   6   7   8
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

pieza(reina, pos(negro, 5, 5)).
pieza(rey, pos(negro, 5, 1)).
pieza(torre, pos(negro, 2, 2)).
pieza(alfil, pos(negro, 4, 4)).
pieza(peon, pos(negro, 8, 6)).
pieza(peon, pos(negro, 6, 6)).


%%%% Getters
tipo(pieza(Tipo, _), Tipo).
posicion(pieza(_, Pos), Pos).
color(pos(Color, _, _), Color).
coordX(pos(_, X, _), X).
coordY(pos(_, _, Y), Y).
%%%%

%%%% Colores
enemigo(blanco, negro).
enemigo(negro, blanco).

colorValido(negro).
colorValido(blanco).
%%%%


%%%% Posiciones	
piezaEn(Posicion) :- aliadoEn(Posicion).
piezaEn(Posicion) :- enemigoEn(Posicion).

aliadoEn(Posicion) :- hayPiezaEn(Posicion).

enemigoEn(Posicion) :-
	posEnemiga(Posicion, PosicionEnemiga),
	hayPiezaEn(PosicionEnemiga).
	
posEnemiga(pos(Color, X, Y), pos(ColorEnemigo, XEnemigo, YEnemigo)) :-
	enemigo(Color, ColorEnemigo),
	XEnemigo is 9 - X,
	YEnemigo is 9 - Y.

hayPiezaEn(Posicion) :- pieza(_, Posicion).
	
posValida(pos(Color, X, Y)) :-
	colorValido(Color),
	between(1, 8, X),
	between(1, 8, Y).

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
	
caminoLibre(PosActual, PosFutura, Cond) :-
	call(Cond, PosActual, PosFutura),
	forall((posEntre(PosActual, PosFutura, PosMedio), call(Cond, PosActual, PosMedio)),
		posDisponible(PosActual, PosFutura, PosMedio)).
	
posDisponible(_, _, Pos) :- not(piezaEn(Pos)).
posDisponible(Pos, _, Pos).
posDisponible(_, Pos, Pos) :- enemigoEn(Pos).
	
saltoLibre(PosActual, PosFutura, Cond) :-
	call(Cond, PosActual, PosFutura),
	not(aliadoEn(PosFutura)).
%%%%

%%%% Movimientos
puedeMoverse(Pieza, Posicion) :-
	posValida(Posicion),
	not(posicion(Pieza, Posicion)),
	posibleMov(Pieza, Posicion).

posibleMov(pieza(alfil, PosActual), PosFutura) :- caminoLibre(PosActual, PosFutura, movimientoAlfil).

posibleMov(pieza(caballo, PosActual), PosFutura) :- saltoLibre(PosActual, PosFutura, movimientoCaballo).
		
posibleMov(pieza(peon, PosActual), PosFutura) :- saltoLibre(PosActual, PosFutura, movimientoPeon).

posibleMov(pieza(reina, PosActual), PosFutura) :- posibleMov(pieza(torre, PosActual), PosFutura).
posibleMov(pieza(reina, PosActual), PosFutura) :- posibleMov(pieza(alfil, PosActual), PosFutura).

posibleMov(pieza(rey, PosActual), PosFutura) :- saltoLibre(PosActual, PosFutura, movimientoRey).
	
posibleMov(pieza(torre, PosActual), PosFutura) :- caminoLibre(PosActual, PosFutura, movimientoTorre).
		
%Peon	
movimientoPeon(pos(Color, X, 2), pos(Color, X, 4)).
movimientoPeon(pos(Color, X, Y), pos(Color, X, PY)) :-
	PY is Y + 1.
movimientoPeon(pos(Color, X, Y), Posicion) :-
	posValida(Posicion),
	Posicion = pos(Color, PX, PY),
	distanciaEntre(X, PX, 1),
	PY is Y + 1,
	enemigoEn(Posicion).

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
	distanciaEntre(X, DX, Dist),
	distanciaEntre(Y, DY, Dist).
	
distanciaEntre(X, Y, Dist) :-
	comp(mod, subs(X), Y, Dist).
%%%
