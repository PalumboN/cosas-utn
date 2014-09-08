% file:///C:/Users/Palumbo/Downloads/Parcial%20L%C3%B3gico%202014-07-10.pdf

:- module(sturbacks, 
	[ingrediente/2, caloriasTotales/2, bebidaLight/1, esAlcoholica/1,
	tieneProblemitas/1, redDeGustos/2]).


% 1)
ingrediente(Bebida, Ingrediente) :- 
	bebida(Bebida, Ingredientes),
	member(Ingrediente, Ingredientes).

% 2)
caloriasTotales(Bebida, CaloriasTotales) :-
	bebida(Bebida, _),
	findall(Calorias, 
		(ingrediente(Bebida, Ingrediente), calorias(Ingrediente, Calorias)),
		ListaCalorias),
	sumlist(ListaCalorias, CaloriasTotales).
	
calorias(base(Base, Cantidad), Calorias) :- 
	Base \= cafe,
	Calorias is Cantidad * 10.
calorias(base(cafe, Cantidad), Calorias) :- Calorias is Cantidad * 2.
calorias(jarabe(_), 10).
calorias(leche(CantGrasa, CantLeche), Calorias) :- Calorias is CantGrasa * CantLeche.

% 3)
bebidaLight(Bebida) :-
	esLiviana(Bebida),
	tieneIngredientesLivianos(Bebida).
	
esLiviana(Bebida) :-
	caloriasTotales(Bebida, Calorias),
	Calorias < 80.

tieneIngredientesLivianos(Bebida) :- 
	forall(ingrediente(Bebida, Ingrediente), esLiviano(Ingrediente)).
	
esLiviano(Ingrediente) :- 
	calorias(Ingrediente, Calorias),
	Calorias =< 15.

% 4)
esAlcoholica(Bebida) :- 
	ingrediente(Bebida, Ingrediente),
	esAlcoholico(Ingrediente).

esAlcoholico(base(Base,_)) :- tieneAlcohol(Base).
esAlcoholico(jarabe(Jarabe)) :- tieneAlcohol(Jarabe).

% 5)
tieneProblemitas(Cliente) :-
	pedido(Cliente, _),
	bebidaLight(BebidaLight),
	not(pedido(Cliente, BebidaLight)),
	esProblematico(Cliente).
	
esProblematico(Cliente) :- 
	pedido(Cliente, Bebida),
	poseeMuchosIngredientes(Bebida).

esProblematico(Cliente) :- 
	forall(esAlcoholica(Bebida), pedido(Cliente, Bebida)).

poseeMuchosIngredientes(Bebida) :-
	bebida(Bebida, Ingredientes),
	length(Ingredientes, Cantidad),
	Cantidad > 10.
	
% 6)
redDeGustos(UnCliente, OtroCliente) :-
	pedido(UnCliente, _),
	pedido(OtroCliente, _),
	UnCliente \= OtroCliente,
	seEncuentranEnLaMismaRed(UnCliente, OtroCliente).
	
seEncuentranEnLaMismaRed(UnCliente, OtroCliente) :-
	pidieronLoMismo(UnCliente, OtroCliente).

seEncuentranEnLaMismaRed(UnCliente, OtroCliente) :-
	hayUnTerceroEnLaRed(UnCliente, OtroCliente, [UnCliente, OtroCliente]).

pidieronLoMismo(UnCliente, OtroCliente) :-
	pedido(UnCliente, Bebida),
	pedido(OtroCliente, Bebida).
	
hayUnTerceroEnLaRed(UnCliente, OtroCliente, Evaluados) :-
	pidieronLoMismo(UnCliente, Tercero),
	not(member(Tercero, Evaluados)),
	evaluarRedCon(OtroCliente, Tercero, [Tercero|Evaluados]).
	
evaluarRedCon(UnCliente, OtroCliente, _) :-
	pidieronLoMismo(UnCliente, OtroCliente).
	
evaluarRedCon(UnCliente, OtroCliente, Evaluados) :-
	hayUnTerceroEnLaRed(UnCliente, OtroCliente, Evaluados).