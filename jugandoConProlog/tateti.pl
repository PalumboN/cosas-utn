:- dynamic jugada/3.
:- use_module(library(clpfd)).

printJugada([Fila1, Fila2, Fila3]) :-
  format("xoxox \n"),
  printFila(Fila1),
  format("_____ \n"),
  printFila(Fila2),
  format("_____ \n"),
  printFila(Fila3),
  format("xoxox \n\n").


printFila([A,B,C]) :-
  printCasillero(A),
  format("|"),
  printCasillero(B),
  format("|"),
  printCasillero(C),
  format("\n").

printCasillero(vacia) :- format(" ").
printCasillero(Jugador) :- format(Jugador).


comenzar :- 
  jugada(vacia, Jugador, JugadaInicial),
  printJugada(JugadaInicial),
  jugadaInteractiva(Jugador, JugadaInicial).

jugadaInteractiva(_, Jugada) :- 
  ganador(Jugada, Ganador), 
  format("Ganador: ~w \n", Ganador),
  asserta(jugada(ganadora, q, Jugada)),
  abort.

jugadaInteractiva(x, Jugada) :-
  not(ganador(Jugada, _)),
  read(Posicion),
  jugar(Jugada, x, Posicion, ProximaJugada),
  printJugada(ProximaJugada),
  jugadaInteractiva(o, ProximaJugada).


jugadaInteractiva(o, Jugada) :-
  not(ganador(Jugada, _)),
  proximaJugada(Jugada, o, ProximaJugada),
  printJugada(ProximaJugada),
  jugadaInteractiva(x, ProximaJugada).



resultado(Partido, JugadaFinal) :-
    jugada(Partido, Jugador, JugadaInicial),
    jugarHastaGanar(JugadaInicial, Jugador, JugadaFinal).

jugarHastaGanar(JugadaInicial, _, JugadaInicial) :-
  ganador(JugadaInicial, _).

jugarHastaGanar(JugadaInicial, Jugador, JugadaFinal) :-
    not(ganador(JugadaInicial, _)),
    proximaJugada(JugadaInicial, Jugador, ProximaJugada),
    otroJugador(Jugador, Enemigo),
    jugarHastaGanar(ProximaJugada, Enemigo, JugadaFinal),
    printJugada(ProximaJugada).

ganador(Jugada, Ganador) :-
    slot(Jugada, [Ganador, Ganador, Ganador]),
    Ganador\=vacia.

slot(Jugada, Slot) :-
    member(Slot, Jugada).
slot(Jugada, Slot) :-
    transpose(Jugada, Transpose),
    member(Slot, Transpose).
slot([[A, _, _], [_, B, _], [_, _, C]], [A, B, C]).
slot([[_, _, C], [_, B, _], [A, _, _]], [A, B, C]).


otroJugador(x, o).
otroJugador(o, x).


proximaJugada(Jugada, Jugador, ProximaJugada) :-
    proximaJugadaGanadora(Jugada, Jugador, _, ProximaJugada).

proximaJugada(Jugada, Jugador, ProximaJugada) :-
    not(proximaJugadaGanadora(Jugada, Jugador, _, _)),
    otroJugador(Jugador, Enemigo),
    proximaJugadaGanadora(Jugada, Enemigo, PosicionPerdedora, _),
    jugar(Jugada, Jugador, PosicionPerdedora, ProximaJugada).

proximaJugada(Jugada, Jugador, ProximaJugada) :-
    not(proximaJugadaGanadora(Jugada, Jugador, _, _)),
    otroJugador(Jugador, Enemigo),
    not(proximaJugadaGanadora(Jugada, Enemigo, _, _)),
    proximaJugadaHaciaElTriunfo(Jugada, Enemigo, PosicionPerdedora, _),
    jugar(Jugada, Jugador, PosicionPerdedora, ProximaJugada).
  
proximaJugada(Jugada, Jugador, ProximaJugada) :-
    not(proximaJugadaGanadora(Jugada, Jugador, _, _)),
    otroJugador(Jugador, Enemigo),
    not(proximaJugadaGanadora(Jugada, Enemigo, _, _)),
    not(proximaJugadaHaciaElTriunfo(Jugada,
                                    Enemigo,
                                    _,
                                    _)),
    proximaJugadaHaciaElTriunfo(Jugada, Jugador, _, ProximaJugada).


proximaJugadaHaciaElTriunfo(Jugada, Jugador, Posicion, ProximaJugada) :-
    jugar(Jugada, Jugador, Posicion, ProximaJugada),
    proximaJugadaGanadora(ProximaJugada, Jugador, _, _).


proximaJugadaGanadora(Jugada, Jugador, Posicion, ProximaJugada) :-
    jugar(Jugada, Jugador, Posicion, ProximaJugada),
    ganador(ProximaJugada, Jugador).


jugar(Jugada, Jugador, Posicion, ProximaJugada) :-
    estaLibre(Posicion, Jugada),
    cartesiano(Posicion, IndiceColumna, IndiceFila),
    nth0(IndiceFila, Jugada, Fila),
    reemplazar(IndiceFila, Jugada, ProximaFila, ProximaJugada),
    reemplazar(IndiceColumna, Fila, Jugador, ProximaFila).


estaLibre(Posicion, Jugada) :-
    cartesiano(Posicion, IndiceColumna, IndiceFila),
    nth0(IndiceFila, Jugada, Fila),
    nth0(IndiceColumna, Fila, vacia).



cartesiano(Posicion, X, Y) :-
    between(1, 9, Posicion),
    Y is (Posicion-1)div 3,
    X is Posicion-Y*3-1.


reemplazar(0, [_, B, C], Nuevo, [Nuevo, B, C]).
reemplazar(1, [A, _, C], Nuevo, [A, Nuevo, C]).
reemplazar(2, [A, B, _], Nuevo, [A, B, Nuevo]).

% TESTS
jugada(vacia, x, [[vacia, vacia, vacia], [vacia, vacia, vacia], [vacia, vacia, vacia]]).
jugada(ganador_fila, x, [[x, x, x], [vacia, vacia, vacia], [vacia, vacia, vacia]]).
jugada(ganador_fila, x, [[vacia, vacia, vacia], [x, x, x], [vacia, vacia, vacia]]).
jugada(ganador_fila, x, [[vacia, vacia, vacia], [vacia, vacia, vacia], [x, x, x]]).
jugada(ganador_columna, x, [[x, vacia, vacia], [x, vacia, vacia], [x, vacia, vacia]]).
jugada(ganador_columna, x, [[vacia, x, vacia], [vacia, x, vacia], [vacia, x, vacia]]).
jugada(ganador_columna, x, [[vacia, vacia, x], [vacia, vacia, x], [vacia, vacia, x]]).

jugada(inicial, o, Jugada) :-
    jugada(vacia, _, Vacia),
    jugar(Vacia, x, 1, Jugada).

jugada(piola, o, Jugada) :-
    jugada(inicial, _, Inicial),
    jugar(Inicial, o, 2, Stage2),
    jugar(Stage2, x, 4, Jugada).

jugadaConGanador(Id, Ganador) :-
    jugada(Id, _, Jugada),
    ganador(Jugada, Ganador).


:- begin_tests(ganador).

test(inicial_no_tiene_ganador, [nondet, fail]) :-
    jugadaConGanador(inicial, _).

test(ganador_por_fila, [nondet]) :-
    forall(jugada(ganador_fila, _, Jugada), ganador(Jugada, x)).

test(ganador_por_fila, [nondet]) :-
    forall(jugada(ganador_columna, _, Jugada), ganador(Jugada, x)).
:- end_tests(ganador).
