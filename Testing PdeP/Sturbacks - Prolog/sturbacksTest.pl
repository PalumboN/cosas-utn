%Se corren con el comando run_tests.

:- use_module(sturbacks).

pedido(vero,frappuccinoFrutilla).
pedido(gus,irishCream).
pedido(gus,explosiva).
pedido(gus,extrema).
pedido(alf,conTodo).
pedido(franco,light).
pedido(franco,irishCream).
pedido(franco,explosiva).
pedido(franco,extrema).
pedido(franco,conTodo).

bebida(dulceDeLecheLatte,[base(cafe,100),leche(10,50)]).
bebida(frappuccinoFrutilla,[base(helado,80),jarabe(frutilla),jarabe(dulceDeLeche),leche(2,60)]).
bebida(irishCream,[base(cafe,90),jarabe(baileys),leche(3,50)]).
bebida(explosiva,[base(ron,90),base(vodka,100),jarabe(frutilla)]).
bebida(extrema,[base(cafe,100),base(helado,80),base(ron,90),leche(10,10),jarabe(chocolate)]).
bebida(light,[base(cafe,5),jarabe(frutilla)]).
bebida(cafesito,[base(cafe,10)]).
bebida(conTodo,[base(cafe,5),jarabe(frutilla),base(helado,80),base(mouse,90),leche(10,10),jarabe(chocolate),base(helado,80),base(mouse,90),leche(10,10),jarabe(chocolate),base(helado,80),base(mouse,90),leche(10,10),jarabe(chocolate)]).

tieneAlcohol(baileys).
tieneAlcohol(tiaMaria).
tieneAlcohol(vodka).
tieneAlcohol(ron).

:- begin_tests(ingrediente).

test(ingredienteEsInversibleRespectoALosIngredientes, 
	[set(Ingrediente == [base(vodka,100),base(ron,90),jarabe(frutilla)])]) :-
        ingrediente(explosiva, Ingrediente).

test(ingredienteEsInversibleRespectoALaBebida,
	[true(Bebida == explosiva)], nondet) :-
        ingrediente(Bebida, base(vodka,100)).
		
:- end_tests(ingrediente).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(caloriasTotales).

test(caloriasDelCafeEsElDobleQueSuCantidad) :-
        sturbacks:calorias(base(cafe, 10), 20).

test(caloriasDeBaseQueNoSeaCafeEsSuCantidadPor10,
	[nondet]) :-
        sturbacks:calorias(base(ron, 10), 100).

test(caloriasDelJarabeEs10) :-
        sturbacks:calorias(jarabe(_), 10).

test(caloriasDeLecheEsCantLechePorCantGrasa) :-
        sturbacks:calorias(leche(5, 10), 50).

test(caloriasTotalesEsLaSumaDeLasCaloriasDeLosIngredientes) :-
        caloriasTotales(extrema, 2010).
		
test(caloriasTotalesEsInversibleRespectoALasCalorias, 
	[true(Calorias == 2010)]) :-
        caloriasTotales(extrema, Calorias).
		
test(caloriasTotalesEsInversibleRespectoALaBebida, 
	[true(Bebida == extrema), nondet]) :-
        caloriasTotales(Bebida, 2010).

:- end_tests(caloriasTotales).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(bebidaLight).
		
test(bebidaNoEsLightCuandoSusCaloriasTotalesSonMayoresQue80,
	[fail]) :-
        bebidaLight(extrema).

test(bebidaNoesLightCuandoAlgunIngredientePoseeCaloriasMayoresA15,
	[fail]) :-
        bebidaLight(cafesito).
		
test(bebidaLightEsInversible, 
	[set(Bebida == [light]), nondet]) :-
        bebidaLight(Bebida).

:- end_tests(bebidaLight).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(esAlcoholica).

test(bebidaEsAlcoholicaCuandoPoseeAlgunComponenteAlcoholico) :-
        assertion(esAlcoholica(irishCream)),
		assertion(esAlcoholica(explosiva)).

test(esAlcoholicaEsInversible, 
	[set(Bebida == [irishCream, explosiva, extrema])]) :-
        esAlcoholica(Bebida).
		
:- end_tests(esAlcoholica).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(tieneProblemitas).

test(clienteQuePidioUnaBebidaConMasDe10IngredientesTieneProblemitas,
	[nondet]) :-
		tieneProblemitas(alf).

test(clienteQuePidioTodasLasBebidasAlcoholicasDelBarTieneProblemitas,
	[nondet]) :-
		tieneProblemitas(gus).
		
test(clienteQuePidioAlgoLightNoTieneProblemitas,
	[fail]) :-
		tieneProblemitas(franco).

test(tieneProblemitasEsInversible,
	[set(Cliente == [alf, gus])]) :-
		tieneProblemitas(Cliente).

:- end_tests(tieneProblemitas).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(redDeGustos).

test(dosClientesCompartenLaRedDeGustosSiPidieronLaMismaBebida,
	[nondet]) :-
		redDeGustos(franco, alf).


test(dosClientesCompartenLaRedDeGustosSiPoseenUnTerceroEnComun,
	[nondet]) :-
		redDeGustos(franco, gus).

test(clienteNoPoseeRedDeGustosSiNadiePidioLaMismaBebida,
	[fail]) :-
		redDeGustos(vero, _).
		
test(redDeGustosEsInversible,
	[set(Cliente == [alf, gus])]) :-
		redDeGustos(franco, Cliente).
		
:- end_tests(redDeGustos).