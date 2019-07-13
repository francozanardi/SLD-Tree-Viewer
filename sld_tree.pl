% Falta analizar el tema del occurs_check y de las sustituciones.
% Una posibilidad para el tema de las sustituciones es manejarlas vía java.


:- use_module(library(aggregate)).
:- dynamic datos/1.

a:- b, d.
a:- c, b.
d:- c, e.
%e:- fail. para hacerlo funcionar tengo que en la consola hacer assertz(e) y después retract(e).
c.
b.


p(X):-
	X > 0,
	q(X).
	
p(X):-
	\+(X > 0),
	r(X).

p(X). %Alternativa que siempre se cumple
	
q(X):-
	0 = 0,
	true,
	0 = 1.
	
q(X).
	
r(X):-
	Lista = [X],
	length(Lista, R),
	append(Lista, Lista, NL),
	length(NL, R2).
	

pertenece(X, [X | Xs]):-
	!.

pertenece(X, [A | Xs]):-
	pertenece(X, Xs).


a(X):-
	X > 0,
	b(X),
	!,
	X = 2.

a(_).

b(1).
b(2).

test :-
    writeln("entrada."),
    between(1, 2, X),
    write("X: "),write(X),nl,
    %0 = 1,
    
    (
		writeln("Sentencia A."),
		X = 1,
		!,
		0 = 1
    ;
		writeln("Sentencia B."),
		X = 2
    ),
    
    writeln(X),
    writeln("Sentencia compartida.").



testReducido :-
    between(1, 2, X),    
    (
		X = 1,
		!,
		0 = 1
    ;
		X = 2
    ),
    
    writeln(X).
	
testIF(X):-
	writeln("entrada."),
	(
		(X = 0) ->
			writeln("IF"),
			X = 0
		;
			writeln("Else")
	),
	writeln("salida.").
	

t1:-
	(X = 0 ->  writeln("IF"), X = 1);
	writeln("salida.").

f(1).	
f(2).	

foo :-
    (  false
    -> write('not gonna happen'), nl
    ;  f(X),
       write(X), nl
    ).


t2:-
    writeln("entrada"),
    between(1, 2, X),
    write("X: "),write(X),nl,
    %0 = 1,
    
    (false ->
		writeln("Sentencia A")
    ;
		writeln("Sentencia B"),
		X = 2
    ),
    
    writeln("Sentencia compartida").

x:- z, y; new.
z:- 0=1;0=0.
new:- 0>2;0=0,0=1,true;5+5=5+5.

trep:-
	repeat,
	0=0.
	
trep2:-
	repeat,
	true,
	repeat,
	!,
	fail.

% La idea es hacer un meta-intérprete capaz de realizar un árbol SLD para un programa P.
% Se hacen uso de assertz y retract para no verse afectado por el backtracking del meta-intérprete y conservar todo nodo,
% de esta manera podremos ver los nodos que llevaron a caminos fallidos y como fue el árbol completo.
% Además no se acceden a predicados predefinidos por el sistema (built-in) y solo se evalúa la veracidad de estos.
% crearSLD/2 es el predicado encargado de crear el árbol para una consulta ingreada en su primer argumento.

% Nueva implementación.
% arbol(rama(ID, fotogramaAparicion, IDNodoP, IDNodoH, FotogramaCut)) % Si IDNodoH = -1, entonces éste aún no se conoce. Si FotogramaCut = -1 entonces no hay cut.
% arbol(nodo(ID, IDPadre, fotogramaAparicion, rotulo))

% Llevamos con assertz y retract la última ID dada a un nodo y el fotograma actual.

% La idea para el cut es reconocerlo y apartir de ahí buscar toda rama que se necesite modificar.

datos(fotogramaActual(1)).
datos(ultimaID_nodo(0)).
datos(ultimaID_rama(0)).

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
	

tieneCut(!).
	
tieneCut((A; B)):-
	tieneCut(A);
	tieneCut(B).

tieneCut((A, B)):-
	tieneCut(A);
	tieneCut(B).

hayCutEnLista([X | Xs]):-
	tieneCut(X),
	!.

hayCutEnLista([X | Xs]):-
	hayCutEnLista(Xs).
	

resolverCut(nodo(_, _, _, Rotulo)):-
	\+(hayCutEnLista(Rotulo)),
	!.
	
resolverCut(nodo(_, IDPadre, _, _)):-
	arbol(nodo(IDPadre, IDAbuelo, FotPadre, RotuloPadre)),
	
	forall(	
				arbol(rama(RamaID, RamaFot, IDPadre, -1, -1)), 
				(
					retract(arbol(rama(RamaID, RamaFot, IDPadre, -1, -1))),
					datos(fotogramaActual(FActual)),
					assertz(arbol(rama(RamaID, RamaFot, IDPadre, -1, FActual)))
				)
			),
			
	resolverCut(nodo(IDPadre, IDAbuelo, FotPadre, RotuloPadre)).

% Resuelve sentencia If - then - else ( IF -> THEN ; ELSE) utilizando la definición de swipl.
% De esta manera se hace visible su desarrollo en el árbol.

% https://www.cs.bham.ac.uk/research/projects/poplog/doc/prologhelp/conditional
% Según esa fuente Prolog define el ; (or) de la siguiente manera:
% 	(X -> Y); Z  :-  X, !, Y.
% 	(X -> Y); Z  :- !, Z. % este cut es para que no se genere otra solución alternativa y se intente unificar con las dos soluciones de abajo. (X sería igual a (X' -> Y'))
% 	X ; Y        :- X.
%	X ; Y        :- Y.
%	
	
solve((A -> B ; C), nodo(IDPadre, _, _, [(A -> B ; C) | RotuloRestante])):-
	!,
	agregarRama(IDPadre, -1, RamaThen),
	agregarRama(IDPadre, -1, RamaElse),
	
	(
		conjuncionesALista((A, !, B), Conjunciones),
		append(Conjunciones, RotuloRestante, Rotulo),
		agregarNodo(Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(RamaThen, ID),
		aumentarFotogramaActual,
		
		solve((A, !, B), NodoAgregado)
		
	;
		RamaElse = rama(IDRamaElse, _, _, _, _),
		arbol(rama(IDRamaElse, _, _, _, -1)), % verificamos que la rama no haya sido podada.
		
		conjuncionesALista(C, Conjunciones), % acá sacamos el ! de (!, C) ya que no hace falta en nuestro caso (si bien prolog si lo utiliza).
		append(Conjunciones, RotuloRestante, Rotulo),
		agregarNodo(Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(RamaElse, ID), 
		aumentarFotogramaActual,
		
		solve(C, NodoAgregado) % acá sacamos el ! de (!, C) ya que no hace falta en nuestro caso (si bien prolog si lo utiliza).
	).

% Resuelve sentencia ';' utilizando la definición de swipl.
% De esta manera se hace visible su desarrollo en el árbol.
solve((A; B), nodo(IDPadre, _, _, [(A; B) | RotuloRestante])):-
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
		solve(A, NodoAgregado)
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
		solve(B, NodoAgregado)
	).
	

solve((A | B), Nodo):-
	!,
	solve((A ; B), Nodo).

% Resuelve sentencia If - then ( IF -> THEN) utilizando la definición de swipl.
% De esta manera se hace visible su desarrollo en el árbol.
solve((A -> B), nodo(IDPadre, _, _, [(A -> B) | RotuloRestante])):-
	!,
	conjuncionesALista((A, !, B), Conjunciones),
	append(Conjunciones, RotuloRestante, Rotulo),
	agregarNodo(Rotulo, IDPadre, NodoAgregado),
	NodoAgregado = nodo(ID, _, _, _),
	agregarRama(IDPadre, ID, _),
	aumentarFotogramaActual,
	
	solve((A, !, B), NodoAgregado).
	

% En este caso tenemos una conjunción de reglas (o hechos), por lo tanto accesamos a cada uno.
solve((A, B), NodoPadre):-
	!,
    solve(A, NodoPadre),
	
	% Aquí tomamos el nodo con mayor ID ya que, A podría haber sido una regla y tomar muchos nodos para solucionarse.
	% Si se está aquí entonces A se resolvió, y el nodo con ID mayor es quien lo resolvió, entonces este nodo será el padre 
	% de las conjunciones restantes.
	getMaxID(UltimaID),
	arbol(nodo(UltimaID, IDP, Fot, Conj)),
    solve(B, nodo(UltimaID, IDP, Fot, Conj)).


% A es UNA ÚNICA regla (o hecho) definida por el usuario donde tiene al menos una posible solución.
solve(A, nodo(IDPadre, IDAbuelo, FotPadre, [A | ConjuncionesRestantes])):-

    % A \= true,
    % A \= (_, _),
	% Verificamos que el predicado no sea uno provisto por el sistema (nodebug), en caso de serlo es privado y no podemos acceder a su cuerpo
	\+(predicate_property(A, nodebug)),
	
	% Arriba de conjuncionesALista/3 y clause/2 tenemos que agregar las ramas, una para cada posible solución.
	% Para ello utilizamos: aggregate_all(count, (clause(A, BP), conjuncionesALista(BP, _, _)), C).
	% Luego agregamos C ramas.
		
	aggregate_all(count, clause(A, _), C), 
	C > 0,
	
	datos(ultimaID_rama(IDRamaActual)),
	IDPrimeraRama is IDRamaActual+1, %Guardamos el ID de la primera rama.
	
	agregarRamas(C, IDPadre),
	
	datos(ultimaID_rama(IDUltimaRama)), %Guardamos el ID de la última rama colocada.
	
	!, % Si C > 0, entonces existe algún cuerpo posible, borramos toda solución alternativa de 'solve' para mayor eficiencia.
    clause(A, B),
	
	buscarRamaLibre(RamaLibre, IDPrimeraRama, IDUltimaRama), 
	% buscamos la rama libre que ocupará el nuevo nodo.
	% Si las ramas han sido podadas entonces no serán tenidas en cuenta como libres.
	
	(
		% A es un hecho definido por el usuario y se satisface, por lo tanto el nuevo rótulo será el del padre sin A.
		B = true,
		
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
	
		% Solución posible al problema planteado arriba: usar un if con B = true, si es distinto hacemos esto,
		% sino, si es igual, hacemos lo que corresponda en el caso de un hecho.
		B \= true,
		
			% BR contiene conjunciones obtenidads a partir de B.
			% BResultado tiene esas mismas conjunciones pero en una lista.
			% Ejemplo:
			% 			B = (a, b; c)
			%
			%			BR = (a, b)
			%			BResultado = [a, b]
			%			; (si pedimos mas soluciones)
			%			BR = c
			%			BResultado = [c]
			%			(acá terminan las soluciones)
		% conjuncionesALista(B, BR, BResultado),
		
		conjuncionesALista(B, BResultado),
		
		% Luego, acá, abajo de conjuncionesALista/3, tenemos que asociar el nodo agregado con la rama que corresponda.
		% La rama que corresponda será aquella que tenga menor ID y como NO tenga nodo hijo asociado.
		
		append(BResultado, ConjuncionesRestantes, Rotulo),
		agregarNodo(Rotulo, IDPadre, NodoAgregado),
		% Agregamos un nodo con rótulo igual a la concatenación del cuerpo de A con el rótulo del padre de A sin A.
		% Es decir reemplazamos A por su cuerpo.
		
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(RamaLibre, ID),
		
		% Acá buscamos la UltimaID agregada y no hacemos IDPadre+1, esto es porque si hay backtracking porque falla alguna conjunción restante
		% y además de eso hay otras reglas que pueden instanciarse de distinta manera, lo que implicaría que aquí podríamos tomar otro camino.
		% Por eso mismo nosotros comenzamos nuestro camino con una ID+1 de la última ID que falló.
		% Notar que nuestro padre será IDPadre, ya que por tomar otro camino no deberíamos cambiar de padre.
		
		write(A),
		write(" es la cabeza de una regla con cuerpo "),
		write(B),
		nl,
		aumentarFotogramaActual, % esto tiene que hacerse antes del solve.
		solve(B, NodoAgregado)
	).



% En caso de que A sea un predicado provisto por el sistema, por lo que no podemos acceder a su cuerpo. Por ello solo lo quitamos en el rótulo sin accederlo.
% Además A tiene al menos una solución.
solve(A, nodo(IDPadre, IDAbulo, FotPadre, [A | ConjuncionesRestantes])):-
	% A \= (_, _),
	predicate_property(A, nodebug),
	
	(
		A \= repeat,
		
		aggregate_all(count, A, C), % evaluar la posibilidad de que A sea un repeat!.
		C > 0,
		
		datos(ultimaID_rama(IDRamaActual)),
		IDPrimeraRama is IDRamaActual+1, %Guardamos el ID de la primera rama colocada.
		
		agregarRamas(C, IDPadre),
		
		datos(ultimaID_rama(IDUltimaRama)), %Guardamos el ID de la última rama colocada.
		
		!, % Si C > 0, entonces existe algún cuerpo posible, borramos toda solución alternativa de 'solve' para mayor eficiencia.
		A, %simplemente invocamos A para ver si se satisface.
		
		% buscamos la rama libre que será en la cual se agregará el nodo, si la rama fue podada entonces no es tenida en cuenta.
		buscarRamaLibre(RamaLibre, IDPrimeraRama, IDUltimaRama)
	;
		A = repeat,
		crearRamaRepeat(IDPadre, -1, IDRepeat),
		!,
		repeat,
		buscarRamaRepeat(IDRepeat, RamaLibre),
		
		(
			RamaLibre = rama(_, _, _, _, -1) % verificamos que la rama libre no haya sido podada.
		;
			RamaLibre \= rama(_, _, _, _, -1), % verificamos que la rama libre no haya sido podada.
			!, % en caso de que la rama haya sido podada, entonces eliminamos toda solución alternativa del repeat y forzamos un fallo
			fail
		),
		
		agregarRamaRepeat(IDPadre, -1, IDRepeat) % agregamos una rama extra si no fue podada, la cual será la del próximo backtracking.
	),
	
	
	% Esto después resolverlo con un predicado aparte y usando cuts.
	(
		A = !,
		resolverCut(nodo(IDPadre, IDAbulo, FotPadre, [A | ConjuncionesRestantes])),
		
		write(A),
		write(" es un cut (!), por ello podamos las ramas que correspondan."),
		nl
	;
		A \= !
	),
	
	agregarNodo(ConjuncionesRestantes, IDPadre, nodo(ID, _, _, _)),
	
	cambiarHijoRama(RamaLibre, ID),
	aumentarFotogramaActual,
	
	write(A),
	write(" se cumple, pero es built-in, por ello no accedemos."),
	nl.

% En caso de que A esté definida por el usuario y no se satisfaga entonces backtracking.
solve(A, nodo(IDPadre, _, _, _)):-
	% A \= (_, _),
	\+(predicate_property(A, nodebug)),
	\+(clause(A, _)), % si A no se puede satisfacer con nada entonces falla.
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
solve(A, nodo(IDPadre, _, _, _)):-
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


crearSLD(A, Lista):-
	conjuncionesALista(A, Rotulo),
	assertz(arbol(nodo(0, -1, 0, Rotulo))), % Agregamos la raiz del árbol en el fotograma 0.
	solve(A, nodo(0, -1, 0, Rotulo)),
	findall(ElementoArbol, (arbol(ElementoArbol), writeln(ElementoArbol)), Lista).





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
	forall(arbol(P), retract(arbol(P))),
	retractall(arbol(_)),
	retractall(datos(_)),

	
	assertz(datos(fotogramaActual(1))),
	assertz(datos(ultimaID_nodo(0))),
	assertz(datos(ultimaID_rama(0))).
	