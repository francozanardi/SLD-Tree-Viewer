% Falta analizar el tema del occurs_check y de las sustituciones.
% Una posibilidad para el tema de las sustituciones es manejarlas vía java.

:- use_module(library(aggregate)).
:- dynamic datos/1.


% La idea es hacer un meta-intérprete capaz de realizar un árbol SLD para un programa P.
% Se hacen uso de assertz y retract para no verse afectado por el backtracking del meta-intérprete y conservar todo nodo,
% de esta manera podremos ver los nodos que llevaron a caminos fallidos y como fue el árbol completo.
% Además no se acceden a predicados predefinidos por el sistema (built-in) y solo se evalúa la veracidad de estos.
% crearSLD/3 es el predicado encargado de crear el árbol para una consulta ingreada en su primer argumento.

% Nueva implementación.
% arbol(rama(ID, fotogramaAparicion, IDNodoP, IDNodoH, FotogramaCut)) % Si IDNodoH = -1, entonces éste aún no se conoce. Si FotogramaCut = -1 entonces no hay cut.
% arbol(nodo(ID, IDPadre, fotogramaAparicion, rotulo))

% Llevamos con assertz y retract la última ID dada a un nodo y el fotograma actual.

% La idea para el cut es reconocerlo y apartir de ahí buscar toda rama que se necesite modificar.

agregarNodo(Rotulo, IDNodoP, nodo(IDnew, IDNodoP, Fot, Rotulo)):-
	datos(fotogramaActual(Fot)),
	datos(ultimaID_nodo(ID)),
	IDnew is ID+1,
	assertz(arbol(nodo(IDnew, IDNodoP, Fot, Rotulo))), % Agregamos efectivamente el nodo.
	
	%Actualizamos los datos correspondientes.
	retract(datos(ultimaID_nodo(ID))),
	assertz(datos(ultimaID_nodo(IDnew))).
	

	
aumentarFotogramaActual:-
	retract(datos(fotogramaActual(F))),
	NF is F+1,
	assertz(datos(fotogramaActual(NF))).
	
agregarRama(NodoP, NodoH, rama(IDnew, Fot, NodoP, NodoH, -1)):-
	datos(fotogramaActual(Fot)),
	datos(ultimaID_rama(ID)),
	IDnew is ID+1,
	assertz(arbol(rama(IDnew, Fot, NodoP, NodoH, -1))), % Agregamos efectivamente el nodo.
	
	%Actualizamos los datos correspondientes.
	retract(datos(ultimaID_rama(ID))),
	assertz(datos(ultimaID_rama(IDnew))).
	
agregarRamas(Cant, NodoP):-
	forall( between(1, Cant, _), agregarRama(NodoP, -1, _)).


% Se crea una primera rama de un repeat y con ello se utiliza esa ID de la rama como ID del repeat.
% crearRamaRepeat(+NodoP, +NodoH, -IDRepeat)
crearRamaRepeat(NodoP, NodoH, IDRepeat):-
	agregarRama(NodoP, NodoH, rama(IDRepeat, _, _, _, _)),
	assertz(datos(ultimaRamaRepeat(IDRepeat, IDRepeat))). % guardamos el ID de la ultima rama guardada, que en este caso como es la primera será el IDRepeat

% A partir de un ID de un repeat se agrega una rama en este repeat.
% crearRamaRepeat(+NodoP, +NodoH, +IDRepeat)
agregarRamaRepeat(NodoP, NodoH, IDRepeat):-
	retract(datos(ultimaRamaRepeat(IDRepeat, _))),
	agregarRama(NodoP, NodoH, rama(IDUltimaRama, _, _, _, _)),
	assertz(datos(ultimaRamaRepeat(IDRepeat, IDUltimaRama))). % mantenemos el IDRepeat, que es el ID de la primera rama que agregó ese repeat.
	
buscarRamaRepeat(IDRepeat, Rama):-
	datos(ultimaRamaRepeat(IDRepeat, IDUltimaRama)),
	arbol(Rama),
	Rama = rama(IDUltimaRama, _, _, _, _).
	
buscarRamaLibre(Rama, IDMin, IDMax):-
	arbol(Rama),
	esMenorID(Rama, IDMin, IDMax),
	!.
	
esMenorID(rama(ID1, _, _, -1, -1), IDMin, IDMax):-
	ID1 >= IDMin,
	ID1 =< IDMax,
	forall((arbol(rama(ID2, _, _, -1, -1)), ID2 \= ID1, ID2 >= IDMin, ID2 =< IDMax), ID1 < ID2).
	
cambiarHijoRama(rama(ID, Fot, NodoP, NodoH, Cut), NodoH_nuevo):-
	retract(arbol(rama(ID, Fot, NodoP, NodoH, Cut))),
	assertz(arbol(rama(ID, Fot, NodoP, NodoH_nuevo, Cut))).
	

% Cuenta la cantidad de cuts en la expresión recibida.
% Para dividir la expresión solo tiene en cuenta los predicados que el programa reconoce.
getCantidadCuts(!, 1):- !.

getCantidadCuts(F, C):-
	(
		F = (A , B);
		F = (A ; B);
		F = (A -> B);
		F = (A | B);
		F = (A *-> B)
	),
	% Solo me interesa distinguir los predicados que reconoce el programa, es decir,
	% aquellos que son built-in y son simulados, si el programa no lo reconociera, entonces no me interesa ver los cuts que contiene.

	% ejemplo: 	Si el programa no reconciece el predicado ->/2 entonces no me interesaría contar los cuts en sus parámetros.
	%			Por lo cual, en este ejemplo, (true -> 0=0, !, fail), tendría 0 cuts.
	% El motivo por el cual no necesario tener en cuenta esos cuts, es que si el programa no simula tal predicado, entonces,
	% el cut analizado nunca podría ser origen de este predicado, ya que el programa tomaría (true -> 0=0, !, fail) como nodebug
	% y lo analizaría completamente, sin simular su árbol. Por ello mismo su cut nunca sería tenido en cuenta en nuestro árbol.
	
	!,
	getCantidadCuts(A, CA),
	getCantidadCuts(B, CB),
	C is CA+CB.
	
getCantidadCuts(_, 0).


contarCuts([], 0):- !.

contarCuts([X | Xs], C):-
	getCantidadCuts(X, CX),
	contarCuts(Xs, Caux),
	C is Caux+CX.


resolverCut(nodo(ID, IDPadre, Fot, Rotulo)):-
	contarCuts(Rotulo, CantCuts),
	resolverCut(nodo(ID, IDPadre, Fot, Rotulo), CantCuts).

resolverCut(nodo(_, _, _, Rotulo), C):-
	contarCuts(Rotulo, CantCuts),
	CantCuts < C,
	!.
	
resolverCut(nodo(_, IDPadre, _, _), C):-
	arbol(nodo(IDPadre, IDAbuelo, FotPadre, RotuloPadre)), % obtenemos el padre del nodo.
	
	forall(	
				arbol(rama(RamaID, RamaFot, IDPadre, -1, -1)), 
				(
					retract(arbol(rama(RamaID, RamaFot, IDPadre, -1, -1))),
					datos(fotogramaActual(FActual)),
					assertz(arbol(rama(RamaID, RamaFot, IDPadre, -1, FActual)))
				)
			),
			
	resolverCut(nodo(IDPadre, IDAbuelo, FotPadre, RotuloPadre), C).
	
evaluarCutEnA(!, Nodo):-
	!,
	resolverCut(Nodo).

evaluarCutEnA(_, _).

% Resuelve sentencia If - then - else ( IF -> THEN ; ELSE) utilizando la definición de swipl.
% De esta manera se hace visible su desarrollo en el árbol.

% https://www.cs.bham.ac.uk/research/projects/poplog/doc/prologhelp/conditional
% Según esa fuente Prolog define el ; (or) de la siguiente manera:
% 	(X -> Y); Z  :-  X, !, Y.
% 	(X -> Y); Z  :- !, Z. % este cut es para que no se genere otra solución alternativa y se intente unificar con las dos soluciones de abajo. (X sería igual a (X' -> Y'))
% 	X ; Y        :- X.
%	X ; Y        :- Y.
%	
	
solve((A -> B ; C), nodo(IDPadre, _, _, [(A -> B ; C) | RotuloRestante]), ModuleName):-
	!,
	agregarRama(IDPadre, -1, RamaThen),
	agregarRama(IDPadre, -1, RamaElse),
	
	% estas dos ramas es por la definición de ;/2.
	agregarRama(IDPadre, -1, _), 
	agregarRama(IDPadre, -1, _),
	
	(
		conjuncionesALista((A, !, B), Conjunciones),
		append(Conjunciones, RotuloRestante, Rotulo),
		agregarNodo(Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(RamaThen, ID),
		aumentarFotogramaActual,
		
		solve((A, !, B), NodoAgregado, ModuleName)
		
	;
		RamaElse = rama(IDRamaElse, _, _, _, _),
		arbol(rama(IDRamaElse, _, _, _, -1)), % verificamos que la rama no haya sido podada.
		
		conjuncionesALista((!, C), Conjunciones),
		% Podríamos eliminar las dos ramas agregadas y quitar el cut. Sin embargo, dejamos el ! de (!, C) ya que prolog lo utiliza.
		append(Conjunciones, RotuloRestante, Rotulo),
		agregarNodo(Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(RamaElse, ID), 
		aumentarFotogramaActual,
		
		solve((!, C), NodoAgregado, ModuleName)
	).

% Resuelve sentencia ';' utilizando la definición de swipl.
% De esta manera se hace visible su desarrollo en el árbol.
solve((A; B), nodo(IDPadre, _, _, [(A; B) | RotuloRestante]), ModuleName):-
	!,
	agregarRama(IDPadre, -1, RamaA),
	agregarRama(IDPadre, -1, RamaB),
	
	% Creamos DOS ramas.
	
    (
		% Creamos un nodo con A y enviamos ese nodo como padre.
		conjuncionesALista(A, ConjuncionesA),
		append(ConjuncionesA, RotuloRestante, Rotulo),
		agregarNodo(Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(RamaA, ID),
		aumentarFotogramaActual,
		solve(A, NodoAgregado, ModuleName)
	;
		RamaB = rama(IDRamaB, _, _, _, _),
		arbol(rama(IDRamaB, _, _, _, -1)), % verificamos que la rama no haya sido podada.
		
		% Creamos un nodo con B y enviamos ese nodo como padre.
		conjuncionesALista(B, ConjuncionesB),
		append(ConjuncionesB, RotuloRestante, Rotulo),
		agregarNodo(Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(RamaB, ID),
		aumentarFotogramaActual,
		solve(B, NodoAgregado, ModuleName)
	).
	

solve((A | B), Nodo, ModuleName):-
	!,
	solve((A ; B), Nodo, ModuleName).

% Resuelve sentencia If - then ( IF -> THEN) utilizando la definición de swipl.
% De esta manera se hace visible su desarrollo en el árbol.
solve((A -> B), nodo(IDPadre, _, _, [(A -> B) | RotuloRestante]), ModuleName):-
	!,
	conjuncionesALista((A, !, B), Conjunciones),
	append(Conjunciones, RotuloRestante, Rotulo),
	agregarNodo(Rotulo, IDPadre, NodoAgregado),
	NodoAgregado = nodo(ID, _, _, _),
	agregarRama(IDPadre, ID, _),
	aumentarFotogramaActual,
	
	solve((A, !, B), NodoAgregado, ModuleName).
	

% En este caso tenemos una conjunción de reglas (o hechos), por lo tanto accesamos a cada uno.
solve((A, B), NodoPadre, ModuleName):-
	!,
    solve(A, NodoPadre, ModuleName),
	
	% Aquí tomamos el nodo con mayor ID ya que, A podría haber sido una regla y tomar muchos nodos para solucionarse.
	% Si se está aquí entonces A se resolvió, y el nodo con ID mayor es quien lo resolvió, entonces este nodo será el padre 
	% de las conjunciones restantes.
	getMaxID(UltimaID),
	arbol(nodo(UltimaID, IDP, Fot, Conj)),
    solve(B, nodo(UltimaID, IDP, Fot, Conj), ModuleName).
	
solve((\+ A), nodo(IDPadre, _, _, [(\+ A) | RotuloRestante]), ModuleName):-
	!,
	agregarRama(IDPadre, -1, RamaA),
	agregarRama(IDPadre, -1, RamaB),
	
	(
		conjuncionesALista((A, !, fail), Conjunciones),
		append(Conjunciones, RotuloRestante, Rotulo),
		
		agregarNodo(Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(RamaA, ID),
		
		aumentarFotogramaActual,
		
		solve((A, !, fail), NodoAgregado, ModuleName)
	;
		RamaB = rama(IDRamaB, _, _, _, _),
		arbol(rama(IDRamaB, _, _, _, -1)), % verificamos que la rama no haya sido podada.
		
		agregarNodo(RotuloRestante, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(RamaB, ID),
		
		aumentarFotogramaActual
	).
	
solve(not(A), NodoPadre, ModuleName):-
	!,
	solve((\+ A), NodoPadre, ModuleName).

% A es UNA ÚNICA regla (o hecho) definida por el usuario donde tiene al menos una posible solución.
solve(A, nodo(IDPadre, _, _, [A | ConjuncionesRestantes]), ModuleName):-

	% Verificamos que el predicado no sea uno provisto por el sistema (nodebug), en caso de serlo es privado y no podemos acceder a su cuerpo
	\+(predicate_property(ModuleName:A, nodebug)),
		
	aggregate_all(count, clause(ModuleName:A, _), C), 
	C > 0,
	
	datos(ultimaID_rama(IDRamaActual)),
	IDPrimeraRama is IDRamaActual+1, %Guardamos el ID de la primera rama.
	
	agregarRamas(C, IDPadre),
	
	datos(ultimaID_rama(IDUltimaRama)), %Guardamos el ID de la última rama colocada.
	
	!, % Si C > 0, entonces existe algún cuerpo posible, borramos toda solución alternativa de 'solve' para mayor eficiencia.
    clause(ModuleName:A, B),
	
	buscarRamaLibre(RamaLibre, IDPrimeraRama, IDUltimaRama), 
	% buscamos la rama libre que ocupará el nuevo nodo.
	% Si las ramas han sido podadas entonces no serán tenidas en cuenta como libres.
	
	(
		(B = true) ->
			% A es un hecho definido por el usuario y se satisface, por lo tanto el nuevo rótulo será el del padre sin A.
			
			agregarNodo(ConjuncionesRestantes, IDPadre, nodo(ID, _, _, _)),
			% agregamos un nodo con el mismo rótulo que el nodo padre, pero sin la cabeza del hecho solucionada.
			
			cambiarHijoRama(RamaLibre, ID),
			
			write(A),
			write(" es la cabeza de un hecho."),
			nl,
			aumentarFotogramaActual
		;
			% A es una regla definida por el usuario, donde puede que después de ella se tengan que resolver más reglas que están en conjunción.
			% Por ello al rotulo de su padre, debe quitarse ella y reemplazarse por su cuerpo.
		
			conjuncionesALista(B, BResultado),
			
			append(BResultado, ConjuncionesRestantes, Rotulo),
			agregarNodo(Rotulo, IDPadre, NodoAgregado),
			% Agregamos un nodo con rótulo igual a la concatenación del cuerpo de A con el rótulo del padre de A sin A.
			% Es decir reemplazamos A por su cuerpo.
			
			NodoAgregado = nodo(ID, _, _, _),
			cambiarHijoRama(RamaLibre, ID),
			
			write(A),
			write(" es la cabeza de una regla con cuerpo "),
			write(B),
			nl,
			aumentarFotogramaActual, % esto tiene que hacerse antes del solve.
			solve(B, NodoAgregado, ModuleName)
	).

% Caso especial en el que A es un repeat.
solve(repeat, nodo(IDPadre, _, _, [repeat | ConjuncionesRestantes]), _):-
	crearRamaRepeat(IDPadre, -1, IDRepeat),
	!,
	repeat,
	buscarRamaRepeat(IDRepeat, RamaLibre),
	
	(
		(RamaLibre \= rama(_, _, _, _, -1)) ->
			!, % en caso de que la rama haya sido podada, entonces eliminamos toda solución alternativa del repeat y forzamos un fallo
			fail
		;
			true
	),
	
	agregarRamaRepeat(IDPadre, -1, IDRepeat), % agregamos una rama extra si no fue podada, la cual será la del próximo backtracking.
	
	agregarNodo(ConjuncionesRestantes, IDPadre, nodo(ID, _, _, _)),
	
	cambiarHijoRama(RamaLibre, ID),
	aumentarFotogramaActual,
	
	write(repeat),
	write(" es un repeat, por ello no accedemos y creamos rama alternativa."),
	nl.

% En caso de que A sea un predicado provisto por el sistema, por lo que no podemos acceder a su cuerpo. Por ello solo lo quitamos en el rótulo sin accederlo.
% Además A tiene al menos una solución.
solve(A, nodo(IDPadre, IDAbulo, FotPadre, [A | ConjuncionesRestantes]), ModuleName):-
	predicate_property(ModuleName:A, nodebug),
	
	aggregate_all(count, ModuleName:A, C), % evaluar la posibilidad de que A sea un repeat!.
	C > 0,
	
	datos(ultimaID_rama(IDRamaActual)),
	IDPrimeraRama is IDRamaActual+1, %Guardamos el ID de la primera rama colocada.
	
	agregarRamas(C, IDPadre),
	
	datos(ultimaID_rama(IDUltimaRama)), %Guardamos el ID de la última rama colocada.
	
	!, % Si C > 0, entonces existe algún cuerpo posible, borramos toda solución alternativa de 'solve' para mayor eficiencia.
	ModuleName:A, %simplemente invocamos A para ver si se satisface.
	
	% buscamos la rama libre que será en la cual se agregará el nodo, si la rama fue podada entonces no es tenida en cuenta.
	buscarRamaLibre(RamaLibre, IDPrimeraRama, IDUltimaRama),

	evaluarCutEnA(A, nodo(IDPadre, IDAbulo, FotPadre, [A | ConjuncionesRestantes])),
	
	agregarNodo(ConjuncionesRestantes, IDPadre, nodo(ID, _, _, _)),
	
	cambiarHijoRama(RamaLibre, ID),
	aumentarFotogramaActual,
	
	write(A),
	write(" se cumple, pero es built-in, por ello no accedemos."),
	nl.

% En caso de que A esté definida por el usuario y no se satisfaga entonces backtracking.
solve(A, nodo(IDPadre, _, _, _), ModuleName):-
	\+(predicate_property(ModuleName:A, nodebug)),
	\+(clause(ModuleName:A, _)), % si A no se puede satisfacer con nada entonces falla.
	!, % por cuestiones de eficiencia.
	
	agregarNodo([fail], IDPadre, nodo(ID, _, _, _)),
	agregarRama(IDPadre, ID, _),
	aumentarFotogramaActual,
	
    write("Falla "),
    write(A),
    write(" entonces backtracking."),
    nl,
    fail.
	
% En caso de que A sea provista por el sistema (built-in) y no se satisfaga entonces backtracking.
solve(A, nodo(IDPadre, _, _, _), _):-
	% A \= (_, _),
	% predicate_property(A, nodebug),
	% \+(A),
	% acá la condición junto al cut no son necesarias, dado a que no hay más alternativas y las condiciones son excluyentes y extensivas (i.e.: abarcan todas las posibilidades)
	
	agregarNodo([fail], IDPadre, nodo(ID, _, _, _)),
	agregarRama(IDPadre, ID, _),
	aumentarFotogramaActual,
	
    write("Falla "),
    write(A),
    write(" built-in, entonces backtracking."),
    nl,
    fail.

% Path debe ser el path en donde se encuentra ModuleName
% Si el archivo a cargar se encuentra en la misma ubicación donde se encuentra el programa entonces
% ModuleName será igual a nombreArchivo y Path igual a nombreArchivo.pl.
% En caso querer utilizar la ubicación actual en la que se ejecuta el programa, y a partir de esta,
% ubicar el archivo ModuleName, entonces Path debería ser de la siguiente forma: 'folderName/.../ModuleName.pl'.
crearSLD(A, ModuleName, Path, ListaArbol):-
	consult(Path),
	
	assertz(datos(fotogramaActual(1))),
	assertz(datos(ultimaID_nodo(0))),
	assertz(datos(ultimaID_rama(0))),
	
	conjuncionesALista(A, Rotulo),
	assertz(arbol(nodo(0, -1, 0, Rotulo))), % Agregamos la raiz del árbol en el fotograma 0.
	solve(A, nodo(0, -1, 0, Rotulo), ModuleName),
	findall(ElementoArbol, arbol(ElementoArbol), ListaArbol),
	eliminarArbol.



conjuncionesALista((A, B), [A | As]):-
	!,
	conjuncionesALista(B, As).
	
conjuncionesALista(A, [A]).







% conjuncionesALista(A, A, Lista):-
	% A \= (_; _),
	% conjuncionesALista(A, Lista).

% conjuncionesALista((A; B), A, Lista):-
	% conjuncionesALista(A, Lista).
	
% conjuncionesALista((A; B), R, Lista):-
	% conjuncionesALista(B, R, Lista).

% conjuncionesALista(Elemento, [Elemento]):-
	% Elemento \= (_, _).

% conjuncionesALista((X, Xs), [X | Lista]):-
	% conjuncionesALista(Xs, Lista).
	


% getMaxID(-MaxID)	
getMaxID(MaxID):-
	arbol(nodo(MaxID, _, _, _)),
	esMayor(nodo(MaxID, _, _, _)),
	!.
	
esMayor(nodo(ID, _, _, _)):-
	forall( (arbol(nodo(IDaux, _, _, _)), ID \= IDaux), IDaux < ID). %Las IDs son únicas.
	
eliminarArbol:-
	% forall(arbol(P), retract(arbol(P))),
	retractall(arbol(_)),
	retractall(datos(_)),

	
	assertz(datos(fotogramaActual(1))),
	assertz(datos(ultimaID_nodo(0))),
	assertz(datos(ultimaID_rama(0))).
	