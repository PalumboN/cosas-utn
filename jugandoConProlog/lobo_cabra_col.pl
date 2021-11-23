/*
https://es.wikipedia.org/wiki/Acertijo_del_lobo,_la_cabra_y_la_col

El acertijo
Hace mucho tiempo un granjero fue al mercado y compró un lobo, una cabra y una col. Para volver a su casa tenía que cruzar un río. El granjero dispone de una barca para cruzar a la otra orilla, pero en la barca solo caben él y una de sus compras.

Si el lobo se queda solo con la cabra se la come, si la cabra se queda sola con la col se la come.

El reto del granjero era cruzar él mismo y dejar sus compras a la otra orilla del río, dejando cada compra intacta. ¿Cómo lo hizo?
*/

respuesta :- resuelveAcertijo(X), print_term(X, _).


resuelveAcertijo(Acciones) :-
  estadoInicial(Inicio),
  estadoFinal(Fin),
  camino(Inicio, Fin, [], Acciones).

camino(Estado, Estado, _, []).
camino(Inicio, Fin, EstadosAnteriores, [Accion | Acciones]) :- 
  transicionValida(Inicio, Intermedio, Accion),
  not(member(Intermedio, EstadosAnteriores)),
  camino(Intermedio, Fin, [Intermedio | EstadosAnteriores], Acciones).

estadoInicial(
  rio(
    [lobo, cabra, col],
    izquierda,
    []
  )
).

estadoFinal(
  rio(
    [],
    derecha,
    [lobo, cabra, col]
  )
).

estadoValido(rio(_, izquierda, Animales)) :- not(alguienEnPeligro(Animales)).
estadoValido(rio(Animales, derecha, _)) :- not(alguienEnPeligro(Animales)).


alguienEnPeligro(Animales) :-
  member(Cazador, Animales),
  member(Presa, Animales),
  come(Cazador, Presa).

come(lobo, cabra).
come(cabra, col).


transicionValida(Anterior, Proximo, Accion) :-
  estadoValido(Anterior),
  transicion(Anterior, Proximo, Accion),
  estadoValido(Proximo).

transicion(
  rio(Izq, BarcaOriginal, Der), 
  rio(Izq, BarcaCruzada, Der), 
  cruzarSolo(BarcaCruzada)
) :-
  opuestos(BarcaOriginal, BarcaCruzada).

transicion(
  rio(Izq, izquierda, Der), 
  rio(NuevaIzq, derecha, NuevaDer), 
  cruzarCon(derecha, Animal)
) :-
  select(Animal, Izq, NuevaIzq),
  select(Animal, NuevaDer, Der). 

transicion(
  rio(Izq, derecha, Der), 
  rio(NuevaIzq, izquierda, NuevaDer), 
  cruzarCon(izquierda, Animal)
) :-
  select(Animal, Der, NuevaDer),
  select(Animal, NuevaIzq, Izq). 
  


opuestos(izquierda, derecha).
opuestos(derecha, izquierda).
