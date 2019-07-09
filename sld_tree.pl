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
	not(X > 0),
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
	
	
x:- z, y; new.
z:- 0=1;0=0.
new:- 0>2;0=0,0=1,true;5+5=5+5.

% La idea es hacer un meta-intérprete capaz de realizar un árbol SLD para un programa P.
% Se hacen uso de assertz y retract para no verse afectado por el backtracking del meta-intérprete y conservar todo nodo,
% de esta manera podremos ver los nodos que llevaron a caminos fallidos y como fue el árbol completo.
% Además no se acceden a predicados predefinidos por el sistema (built-in) y solo se evalúa la veracidad de estos.
% crearSLD/2 es el predicado encargado de crear el árbol para una consulta ingreada en su primer argumento.

% Nueva implementación.
% arbol(rama(ID, fotogramaAparicion, IDNodoP, IDNodoH, cut = (bitCut, fotograma))) % Si IDNodoH = -1, entonces éste aún no se conoce.
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
	
agregarRama(NodoP, NodoH, rama(IDnew, Fot, NodoP, NodoH, (0, 0))):-
	datos(fotogramaActual(Fot)),
	datos(ultimaID_rama(ID)),
	IDnew is ID+1,
	assertz(arbol(rama(IDnew, Fot, NodoP, NodoH, (0,0)))), % Agregamos efectivamente el nodo.
	
	%Actualizamos los datos correspondientes.
	retract(datos(ultimaID_rama(ID))),
	assertz(datos(ultimaID_rama(IDnew))).
	
aumentarFotogramaActual:-
	retract(datos(fotogramaActual(F))),
	NF is F+1,
	assertz(datos(fotogramaActual(NF))).
	
agregarRamas(Cant, NodoP):-
	forall( between(1, Cant, _), agregarRama(NodoP, -1, _)).
	
buscarRamaLibre(Rama, IDMin):-
	arbol(Rama),
	esMenorID(Rama, IDMin),
	!.
	
esMenorID(rama(ID1, _, _, -1, (0,0)), IDMin):-
	ID1 >= IDMin,
	forall((arbol(rama(ID2, _, _, -1, (0, 0))), ID2 \= ID1, ID2 >= IDMin), ID1 < ID2).
	
cambiarHijoRama(rama(ID, Fot, NodoP, NodoH, Cut), NodoH_nuevo):-
	retract(arbol(rama(ID, Fot, NodoP, NodoH, Cut))),
	assertz(arbol(rama(ID, Fot, NodoP, NodoH_nuevo, Cut))).

% En este caso tenemos una conjunción de reglas (o hechos), por lo tanto accesamos a cada uno.
solve((A, B), NodoPadre):-
    solve(A, NodoPadre),
	
	% Aquí tomamos el nodo con mayor ID ya que, A podría haber sido una regla y tomar muchos nodos para solucionarse.
	% Si se está aquí entonces A se resolvió, y el nodo con ID mayor es quien lo resolvió, entonces este nodo será el padre 
	% de las conjunciones restantes.
	getMaxID(UltimaID),
	arbol(nodo(UltimaID, IDP, Fot, Conj)),
    solve(B, nodo(UltimaID, IDP, Fot, Conj)).




% A es una regla (o hecho) definida por el usuario.
solve(A, nodo(IDPadre, _, _, [A | ConjuncionesRestantes])):-

    % A \= true,
    A \= (_, _),
	% Verificamos que el predicado no sea uno provisto por el sistema (built_in), en caso de serlo es privado y no podemos acceder a su cuerpo
	not(predicate_property(A, built_in)),
	
	% Arriba de conjuncionesALista/3 y clause/2 tenemos que agregar las ramas, una para cada posible solución.
	% Para ello utilizamos: aggregate_all(count, (clause(A, BP), conjuncionesALista(BP, _, _)), C).
	% Luego agregamos C ramas.
	
	datos(ultimaID_rama(IDUltimaRama)),
	IDPrimeraRama is IDUltimaRama+1, %Guardamos el ID de la primera rama.
	
	aggregate_all(count, (clause(A, BP), conjuncionesALista(BP, _, _)), C), 
	% Acá tengo un error, estoy agregando una rama por cada solución posible de A, también agregando ramas por los hechos y después me estoy "saltando"
	% estos hechos y estoy usando sus ramas que le corresponden para ramas de las reglas que siguen, esto está MAL. Si A tuviese como soluciones posibles:
	% regla, hecho, regla; el árbol quedaría mal representado: regla, regla, hecho. Además de que cuando vaya al predicado que se encarga de los hechos no 
	% debería agregar ramas, ya que todas se agregaron aquí.
	
	% Solución posible: que este predicado se encargue tanto de las reglas como los hechos.
	agregarRamas(C, IDPadre),
	
    clause(A, B),
	

	
	(
		% A es un hecho definido por el usuario y se satisface, por lo tanto el nuevo rótulo será el del padre sin A.
		B = true,
		agregarNodo(ConjuncionesRestantes, IDPadre, nodo(ID, _, _, _)),
		
		buscarRamaLibre(Rama, IDPrimeraRama),
		cambiarHijoRama(Rama, ID),
		
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
		conjuncionesALista(B, BR, BResultado),
		
		% Luego, acá, abajo de conjuncionesALista/3, tenemos que asociar el nodo agregado con la rama que corresponda.
		% La rama que corresponda será aquella que tenga menor ID y como NO tenga nodo hijo asociado.
		
		append(BResultado, ConjuncionesRestantes, Rotulo),
		agregarNodo(Rotulo, IDPadre, NodoAgregado),
		
		buscarRamaLibre(Rama, IDPrimeraRama),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(Rama, ID),
		
		% Acá buscamos la UltimaID agregada y no hacemos IDPadre+1, esto es porque si hay backtracking porque falla alguna conjunción restante
		% y además de eso hay otras reglas que pueden instanciarse de distinta manera, lo que implicaría que aquí podríamos tomar otro camino.
		% Por eso mismo nosotros comenzamos nuestro camino con una ID+1 de la última ID que falló.
		% Notar que nuestro padre será IDPadre, ya que por tomar otro camino no deberíamos cambiar de padre.
		
		write(A),
		write(" es la cabeza de una regla con cuerpo "),
		write(BR),
		nl,
		aumentarFotogramaActual, % esto tiene que hacerse antes del solve.
		solve(BR, NodoAgregado)
	).



% En caso de que A sea un predicado provisto por el sistema no podemos acceder a su cuerpo, por ello solo lo quitamos en el rótulo sin accederlo.
solve(A, nodo(IDPadre, _, _, [A | ConjuncionesRestantes])):-
	A \= (_, _),
	predicate_property(A, built_in),
	
	datos(ultimaID_rama(IDUltimaRama)),
	IDPrimeraRama is IDUltimaRama+1, %Guardamos el ID de la primera rama.
	aggregate_all(count, A, C),
	agregarRamas(C, IDPadre),
	
	A, %simplemente invocamos A para ver si se satisface.
	
	agregarNodo(ConjuncionesRestantes, IDPadre, nodo(ID, _, _, _)),
	
	buscarRamaLibre(Rama, IDPrimeraRama),
	cambiarHijoRama(Rama, ID),
	aumentarFotogramaActual,
	
    write(A),
    write(" se cumple, pero es built-in, por ello no accedemos."),
    nl.

% En caso de que A esté definida por el usuario y no se satisfaga entonces backtracking.
solve(A, nodo(IDPadre, _, _, _)):-
	A \= (_, _),
	not(predicate_property(A, built_in)),
	not(clause(A, _)), % si A no se puede satisfacer con nada entonces falla.
	
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
	A \= (_, _),
	predicate_property(A, built_in),
	not(A),
	
	agregarNodo([fail], IDPadre, nodo(ID, _, _, _)),
	agregarRama(IDPadre, ID, _),
	aumentarFotogramaActual,
	
    write("Falla "),
    write(A),
    write(" built-in, entonces backtracking."),
    nl,
    fail.


crearSLD(A, Lista):-
	conjuncionesALista(A, ConjuncionesDeA, Rotulo),
	assertz(arbol(nodo(0, -1, 0, Rotulo))), % Agregamos la raiz del árbol en el fotograma 0.
	solve(ConjuncionesDeA, nodo(0, -1, 0, Rotulo)),
	findall(Nodo, (arbol(Nodo), writeln(Nodo)), Lista).


conjuncionesALista(A, A, Lista):-
	A \= (_; _),
	conjuncionesALista(A, Lista).

conjuncionesALista((A; B), A, Lista):-
	conjuncionesALista(A, Lista).
	
conjuncionesALista((A; B), R, Lista):-
	conjuncionesALista(B, R, Lista).

conjuncionesALista(Elemento, [Elemento]):-
	Elemento \= (_, _).

conjuncionesALista((X, Xs), [X | Lista]):-
	conjuncionesALista(Xs, Lista).
	


% getMaxID(-MaxID)	
getMaxID(MaxID):-
	arbol(nodo(MaxID, _, _, _)),
	esMayor(nodo(MaxID, _, _, _)).
	
esMayor(nodo(ID, _, _, _)):-
	forall( (arbol(nodo(IDaux, _, _, _)), ID \= IDaux), IDaux < ID). %Las IDs son únicas.
	
eliminarArbol:-
	forall(arbol(P), retract(arbol(P))),
	
	retract(datos(fotogramaActual(_))),
	retract(datos(ultimaID_nodo(_))),
	retract(datos(ultimaID_rama(_))),
	
	assertz(datos(fotogramaActual(1))),
	assertz(datos(ultimaID_nodo(0))),
	assertz(datos(ultimaID_rama(0))).
	