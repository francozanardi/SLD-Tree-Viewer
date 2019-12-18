% Una posibilidad para el tema de las sustituciones es manejarlas vía java.
% Tener en cuenta el predicado numbervars/3 que permite "numerar" las variables en un término.
% podríamos darle una lista con todos los rótulos, para que nos nombre todas las variables.

:- use_module(library(aggregate)).
:- dynamic datos/2.
:- dynamic arbol/2.

% :- dynamic(unknown(_, fail)).


% La idea es hacer un meta-intérprete capaz de realizar un árbol SLD para un programa P.
% Se hacen uso de assertz y retract para no verse afectado por el backtracking del meta-intérprete y conservar todo nodo,
% de esta manera podremos ver los nodos que llevaron a caminos fallidos y como fue el árbol completo.
% Además no se acceden a predicados predefinidos por el sistema (built-in) y solo se evalúa la veracidad de estos.
% crearSLD/3 es el predicado encargado de crear el árbol para una consulta ingreada en su primer argumento.

% Nueva implementación.
% arbol(ModuleName, rama(ID, fotogramaAparicion, IDNodoP, IDNodoH, FotogramaCut)) % Si IDNodoH = -1, entonces éste aún no se conoce. Si FotogramaCut = -1 entonces no hay cut.
% arbol(ModuleName, nodo(ID, IDPadre, fotogramaAparicion, varRotulo, atomRotulo))
% varRotulo será el rótulo pero con las variables sin instanciar que correspondan
% Sin embargo, en atomRotulo estas variables estarán transformadas a átomos, así podremos tener una visualización
% más sencilla en el visualizador.

% Dentro del meta-intérprete no se utilizará el atomRotulo, por lo tanto siempre usaremos un functor nodo/4 que exclusa a éste.

% Para distinguir un nodo (o rama) de otro se necesita el ID y el ModuleName.

% Llevamos con assertz y retract la última ID dada a un nodo y el fotograma actual.

% La idea para el cut es reconocerlo y apartir de ahí buscar toda rama que se necesite modificar.


agregarNodo(ModuleName, Rotulo, IDNodoP, nodo(IDnew, IDNodoP, Fot, Rotulo)):-
	datos(ModuleName, fotogramaActual(Fot)),
	datos(ModuleName, ultimaID_nodo(ID)),
	% b_getval(ultimoNodoNoDeshecho, nodo(ID, _, _, _)), 
	% no podemos usarlo, dado que si se hace backtracking, esto también se deshace
	% podríamos usar bn_getval/2 para evitar esto... pero, esto provacaría que no conserve la misma representación interna
	% ya que se hace una copia. Por lo tanto no nos sirve para el objetivo original
	
	IDnew is ID+1,
	
	vars_to_atom(Rotulo, AtomRotulo),
	b_setval(ultimoNodoNoDeshecho, nodo(IDnew, IDNodoP, Fot, Rotulo)),
	
	assertz(arbol(ModuleName, nodo(IDnew, IDNodoP, Fot, Rotulo, AtomRotulo))), % Agregamos efectivamente el nodo.

	%Actualizamos los datos correspondientes.
	retract(datos(ModuleName, ultimaID_nodo(ID))),
	assertz(datos(ModuleName, ultimaID_nodo(IDnew))).

	
aumentarFotogramaActual(ModuleName):-
	retract(datos(ModuleName, fotogramaActual(F))),
	NF is F+1,
	assertz(datos(ModuleName, fotogramaActual(NF))).
	
agregarRama(ModuleName, NodoP, NodoH, rama(IDnew, Fot, NodoP, NodoH, -1)):-
	datos(ModuleName, fotogramaActual(Fot)),
	datos(ModuleName, ultimaID_rama(ID)),
	IDnew is ID+1,
	assertz(arbol(ModuleName, rama(IDnew, Fot, NodoP, NodoH, -1))), 
	
	%Actualizamos los datos correspondientes.
	retract(datos(ModuleName, ultimaID_rama(ID))),
	assertz(datos(ModuleName, ultimaID_rama(IDnew))).
	
agregarRamas(ModuleName, Cant, NodoP):-
	forall( between(1, Cant, _), agregarRama(ModuleName, NodoP, -1, _)).


% Se crea una primera rama de un repeat y con ello se utiliza esa ID de la rama como ID del repeat.
% crearRamaRepeat(+NodoP, +NodoH, -IDRepeat)
crearRamaRepeat(ModuleName, NodoP, NodoH, IDRepeat):-
	agregarRama(ModuleName, NodoP, NodoH, rama(IDRepeat, _, _, _, _)),
	assertz(datos(ModuleName, ultimaRamaRepeat(IDRepeat, IDRepeat))). % guardamos el ID de la ultima rama guardada, que en este caso como es la primera será el IDRepeat

% A partir de un ID de un repeat se agrega una rama en este repeat.
% crearRamaRepeat(+NodoP, +NodoH, +IDRepeat)
agregarRamaRepeat(ModuleName, NodoP, NodoH, IDRepeat):-
	retract(datos(ModuleName, ultimaRamaRepeat(IDRepeat, _))),
	agregarRama(ModuleName, NodoP, NodoH, rama(IDUltimaRama, _, _, _, _)),
	assertz(datos(ModuleName, ultimaRamaRepeat(IDRepeat, IDUltimaRama))). % mantenemos el IDRepeat, que es el ID de la primera rama que agregó ese repeat.
	
buscarRamaRepeat(ModuleName, IDRepeat, Rama):-
	datos(ModuleName, ultimaRamaRepeat(IDRepeat, IDUltimaRama)),
	arbol(ModuleName, Rama),
	Rama = rama(IDUltimaRama, _, _, _, _).
	
buscarRamaLibre(ModuleName, Rama, IDMin, IDMax):-
	arbol(ModuleName, Rama),
	esMenorID(ModuleName, Rama, IDMin, IDMax),
	!.
	
esMenorID(ModuleName, rama(ID1, _, _, -1, -1), IDMin, IDMax):-
	ID1 >= IDMin,
	ID1 =< IDMax,
	forall((arbol(ModuleName, rama(ID2, _, _, -1, -1)), ID2 \= ID1, ID2 >= IDMin, ID2 =< IDMax), ID1 < ID2).
	
cambiarHijoRama(ModuleName, rama(ID, Fot, NodoP, NodoH, Cut), NodoH_nuevo):-
	retract(arbol(ModuleName, rama(ID, Fot, NodoP, NodoH, Cut))),
	assertz(arbol(ModuleName, rama(ID, Fot, NodoP, NodoH_nuevo, Cut))).
	

% Cuenta la cantidad de cuts en la expresión recibida.
% Para dividir la expresión solo tiene en cuenta los predicados que el programa reconoce.
getCantidadCuts(!, 1):- !.

getCantidadCuts(F, C):-
	(
		F = (A , B);
		F = (A ; B);
		F = (A -> B);
		F = (A | B)
	),
	% Solo me interesa distinguir los predicados que reconoce el programa, es decir,
	% aquellos que son built-in y son simulados, si el programa no lo reconociera, entonces no me interesa ver los cuts que contiene.
	% el caso del 'not' es distinto, ya que los cuts que contenga como argumento en el momento previo a una llamada, no me interesa contarlos. 

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


resolverCut(ModuleName, nodo(ID, IDPadre, Fot, Rotulo)):-
	contarCuts(Rotulo, CantCuts),
	resolverCut(ModuleName, nodo(ID, IDPadre, Fot, Rotulo), CantCuts).

resolverCut(_, nodo(_, _, _, Rotulo), C):-
	contarCuts(Rotulo, CantCuts),
	CantCuts < C,
	!.
	
resolverCut(ModuleName, nodo(_, IDPadre, _, _), C):-
	arbol(ModuleName, nodo(IDPadre, IDAbuelo, FotPadre, RotuloPadre, _)), % obtenemos el padre del nodo.
	
	forall(	
				arbol(ModuleName, rama(RamaID, RamaFot, IDPadre, -1, -1)), 
				(
					retract(arbol(ModuleName, rama(RamaID, RamaFot, IDPadre, -1, -1))),
					datos(ModuleName, fotogramaActual(FActual)),
					assertz(arbol(ModuleName, rama(RamaID, RamaFot, IDPadre, -1, FActual)))
				)
			),
			
	resolverCut(ModuleName, nodo(IDPadre, IDAbuelo, FotPadre, RotuloPadre), C).
	
evaluarCutEnA(ModuleName, !, Nodo):-
	!,
	resolverCut(ModuleName, Nodo).

evaluarCutEnA(_, _, _).

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
	agregarRama(ModuleName, IDPadre, -1, RamaThen),
	agregarRama(ModuleName, IDPadre, -1, RamaElse),
	
	% estas dos ramas es por la definición de ;/2.
	agregarRama(ModuleName, IDPadre, -1, _), 
	agregarRama(ModuleName, IDPadre, -1, _),
	
	(
		conjuncionesALista((A, !, B), Conjunciones),
		append(Conjunciones, RotuloRestante, Rotulo),
		agregarNodo(ModuleName, Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(ModuleName, RamaThen, ID),
		aumentarFotogramaActual(ModuleName),
		
		solve((A, !, B), NodoAgregado, ModuleName)
		
	;
		RamaElse = rama(IDRamaElse, _, _, _, _),
		arbol(ModuleName, rama(IDRamaElse, _, _, _, -1)), % verificamos que la rama no haya sido podada.
		
		conjuncionesALista((!, C), Conjunciones),
		% Podríamos eliminar las dos ramas agregadas y quitar el cut. Sin embargo, dejamos el ! de (!, C) ya que prolog lo utiliza.
		append(Conjunciones, RotuloRestante, Rotulo),
		agregarNodo(ModuleName, Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(ModuleName, RamaElse, ID), 
		aumentarFotogramaActual(ModuleName),
		
		solve((!, C), NodoAgregado, ModuleName)
	).

% Resuelve sentencia ';' utilizando la definición de swipl.
% De esta manera se hace visible su desarrollo en el árbol.
solve((A; B), nodo(IDPadre, _, _, [(A; B) | RotuloRestante]), ModuleName):-
	!,
	agregarRama(ModuleName, IDPadre, -1, RamaA),
	agregarRama(ModuleName, IDPadre, -1, RamaB),
	
	% Creamos DOS ramas.
	
    (
		% Creamos un nodo con A y enviamos ese nodo como padre.
		conjuncionesALista(A, ConjuncionesA),
		append(ConjuncionesA, RotuloRestante, Rotulo),
		agregarNodo(ModuleName, Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(ModuleName, RamaA, ID),
		aumentarFotogramaActual(ModuleName),
		solve(A, NodoAgregado, ModuleName)
	;
		RamaB = rama(IDRamaB, _, _, _, _),
		arbol(ModuleName, rama(IDRamaB, _, _, _, -1)), % verificamos que la rama no haya sido podada.
		
		% Creamos un nodo con B y enviamos ese nodo como padre.
		conjuncionesALista(B, ConjuncionesB),
		append(ConjuncionesB, RotuloRestante, Rotulo),
		agregarNodo(ModuleName, Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(ModuleName, RamaB, ID),
		aumentarFotogramaActual(ModuleName),
		solve(B, NodoAgregado, ModuleName)
	).
	

solve((A | B), nodo(IDPadre, IDAbuelo, FotPadre, [(A | B) | RotuloRestante]), ModuleName):-
	!,
	solve((A ; B), nodo(IDPadre, IDAbuelo, FotPadre, [(A ; B) | RotuloRestante]), ModuleName).

% Resuelve sentencia If - then ( IF -> THEN) utilizando la definición de swipl.
% De esta manera se hace visible su desarrollo en el árbol.
solve((A -> B), nodo(IDPadre, _, _, [(A -> B) | RotuloRestante]), ModuleName):-
	!,
	conjuncionesALista((A, !, B), Conjunciones),
	append(Conjunciones, RotuloRestante, Rotulo),
	agregarNodo(ModuleName, Rotulo, IDPadre, NodoAgregado),
	NodoAgregado = nodo(ID, _, _, _),
	agregarRama(ModuleName, IDPadre, ID, _),
	aumentarFotogramaActual(ModuleName),
	
	solve((A, !, B), NodoAgregado, ModuleName).
	

% En este caso tenemos una conjunción de reglas (o hechos), por lo tanto accesamos a cada uno.
solve((A, B), NodoPadre, ModuleName):-
	!,
	
    solve(A, NodoPadre, ModuleName),
	
	% Aquí tomamos el nodo con mayor ID ya que, A podría haber sido una regla y tomar muchos nodos para solucionarse.
	% Si se está aquí entonces A se resolvió, y el nodo con ID mayor es quien lo resolvió, entonces este nodo será el padre 
	% de las conjunciones restantes.
	b_getval(ultimoNodoNoDeshecho, UltimoNodo),
	
	% Antes no usabamos variables globales, pero el problema de esto, es que al querer 
	% recuperar el nodo con mayor ID desde el árbol, las variables cambian su representación interna en el rótulo.
	% Esto no tiene inconvenientes en la simulación del árbol, pero al cambiar la representación interna de las variables,
	% se dificulta mucho más seguir el rastro a las variables.
	% Ejemplo:
	% 	Guardabamos nodo(2, 1, 3, [p(_23), q(_23)]), el cual sería el nodo con mayor ID.
	% Al momento de recuperarlo, tendríamos:
	% nodo(2, 1, 3, [p(_24), q(_24)]) 
	% Es decir, Prolog habría modificado la representación interna de las variables no instanciadas.
	% Nota: (_23 y _24 son usado a modo de ejemplo, no necesariamente tendrán esos mismos números).
	
	% Tener en cuenta que al hacer backtracking, UltimoNodo se deshace.
	% por lo tanto no tiene otra utilidad más que esta.
	% Sabemos que, el último nodo, en este caso no habrá sido deshecho con un backtracking
	% dado que si hubiese sido deshecho, no podría estar aquí. 
	% En otras palabras, al llegar aquí, la finalización del anterior 'solve' fue exitosa
	% y el valor de UltimoNodo es correcto, ya que viene de una rama no deshecha.
	% Si hubiese un backtracking, porque B falla, o provocado para generar más soluciones,
	% estaríamos en las mismas condiciones.
	
    solve(B, UltimoNodo, ModuleName).
	
solve((\+ A), nodo(IDPadre, _, _, [(\+ A) | RotuloRestante]), ModuleName):-
	!,
	agregarRama(ModuleName, IDPadre, -1, RamaA),
	agregarRama(ModuleName, IDPadre, -1, RamaB),
	
	(
		conjuncionesALista((A, !, fail), Conjunciones),
		append(Conjunciones, RotuloRestante, Rotulo),

		agregarNodo(ModuleName, Rotulo, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(ModuleName, RamaA, ID),
		
		aumentarFotogramaActual(ModuleName),
		
		solve((A, !, fail), NodoAgregado, ModuleName)
	;
		RamaB = rama(IDRamaB, _, _, _, _),
		arbol(ModuleName, rama(IDRamaB, _, _, _, -1)), % verificamos que la rama no haya sido podada.
		
		agregarNodo(ModuleName, RotuloRestante, IDPadre, NodoAgregado),
		NodoAgregado = nodo(ID, _, _, _),
		cambiarHijoRama(ModuleName, RamaB, ID),
		
		aumentarFotogramaActual(ModuleName)
	).
	
solve(not(A), nodo(IDPadre, IDAbuelo, FotPadre, [not(A) | RotuloRestante]), ModuleName):-
	!,
	solve((\+ A), nodo(IDPadre, IDAbuelo, FotPadre, [(\+ A) | RotuloRestante]), ModuleName).

% A es UNA ÚNICA regla (o hecho) definida por el usuario donde tiene al menos una posible solución.
solve(A, nodo(IDPadre, _, _, [A | ConjuncionesRestantes]), ModuleName):-

	% Verificamos que el predicado no sea uno provisto por el sistema (nodebug), en caso de serlo es privado y no podemos acceder a su cuerpo
	\+(predicate_property(ModuleName:A, nodebug)),
	
	write("Voy a resolver, del usuario: "),write(A),nl,
	
	aggregate_all(count, clause(ModuleName:A, _), C), %cuento la cantidad de soluciones alternativas que tiene A.
	write("Tiene C: "),write(C),nl,
	C > 0,
	
	datos(ModuleName, ultimaID_rama(IDRamaActual)),
	IDPrimeraRama is IDRamaActual+1, %Guardamos el ID de la primera rama.
	
	agregarRamas(ModuleName, C, IDPadre),
	
	datos(ModuleName, ultimaID_rama(IDUltimaRama)), %Guardamos el ID de la última rama colocada.
	
	!, % Si C > 0, entonces existe algún cuerpo posible, borramos toda solución alternativa de 'solve' para mayor eficiencia.
    clause(ModuleName:A, B),
	
	buscarRamaLibre(ModuleName, RamaLibre, IDPrimeraRama, IDUltimaRama), 
	% buscamos la rama libre que ocupará el nuevo nodo.
	% Si las ramas han sido podadas entonces no serán tenidas en cuenta como libres.
	
	(
		(B = true) ->
			% A es un hecho definido por el usuario y se satisface, por lo tanto el nuevo rótulo será el del padre sin A.
			
			agregarNodo(ModuleName, ConjuncionesRestantes, IDPadre, nodo(ID, _, _, _)),
			% agregamos un nodo con el mismo rótulo que el nodo padre, pero sin la cabeza del hecho solucionada.
			
			cambiarHijoRama(ModuleName, RamaLibre, ID),
			
			write(A),
			write(" es la cabeza de un hecho."),
			nl,
			aumentarFotogramaActual(ModuleName)
		;
			% A es una regla definida por el usuario, donde puede que después de ella se tengan que resolver más reglas que están en conjunción.
			% Por ello al rotulo de su padre, debe quitarse ella y reemplazarse por su cuerpo.
		
			conjuncionesALista(B, BResultado),
			
			append(BResultado, ConjuncionesRestantes, Rotulo),
			agregarNodo(ModuleName, Rotulo, IDPadre, NodoAgregado),
			% Agregamos un nodo con rótulo igual a la concatenación del cuerpo de A con el rótulo del padre de A sin A.
			% Es decir reemplazamos A por su cuerpo.
			
			NodoAgregado = nodo(ID, _, _, _),
			cambiarHijoRama(ModuleName, RamaLibre, ID),
			
			write(A),
			write(" es la cabeza de una regla con cuerpo "),
			write(B),
			nl,
			aumentarFotogramaActual(ModuleName), % esto tiene que hacerse antes del solve.
			solve(B, NodoAgregado, ModuleName)
	).

% Caso especial en el que A es un repeat.
solve(repeat, nodo(IDPadre, _, _, [repeat | ConjuncionesRestantes]), ModuleName):-
	crearRamaRepeat(ModuleName, IDPadre, -1, IDRepeat),
	!,
	repeat,
	buscarRamaRepeat(ModuleName, IDRepeat, RamaLibre),
	
	(
		(RamaLibre \= rama(_, _, _, _, -1)) ->
			!, % en caso de que la rama haya sido podada, entonces eliminamos toda solución alternativa del repeat y forzamos un fallo
			fail
		;
			true
	),
	
	agregarRamaRepeat(ModuleName, IDPadre, -1, IDRepeat), % agregamos una rama extra si no fue podada, la cual será la del próximo backtracking.
	
	agregarNodo(ModuleName, ConjuncionesRestantes, IDPadre, nodo(ID, _, _, _)),
	
	cambiarHijoRama(ModuleName, RamaLibre, ID),
	aumentarFotogramaActual(ModuleName),
	
	write(repeat),
	write(" es un repeat, por ello no accedemos y creamos rama alternativa."),
	nl.

% En caso de que A sea un predicado provisto por el sistema, por lo que no podemos acceder a su cuerpo. Por ello solo lo quitamos en el rótulo sin accederlo.
% Además A tiene al menos una solución.
solve(A, nodo(IDPadre, IDAbulo, FotPadre, [A | ConjuncionesRestantes]), ModuleName):-
	predicate_property(ModuleName:A, nodebug),
	
	findall(PA, 
		(
			predicate_property(ModuleName:PAHead, dynamic),
			clause(ModuleName:PAHead, PABody),
			PA = [PAHead, PABody]
		),
	PredDinActuales),

	aggregate_all(count, ModuleName:A, C), % C cuenta la cantidad de soluciones que finalizan en 'true' para A, por lo tanto si C > 0, sabemos que A se cumple al menos una vez.

	findall(PN, 
		(
			predicate_property(ModuleName:PNHead, dynamic),
			clause(ModuleName:PNHead, PNBody),
			PN = [PNHead, PNBody]
		),
	PredDinNuevos),

	
	restablecerPredicadosDinamicos(ModuleName, PredDinActuales, PredDinNuevos),
	
	C > 0,
	
	
	datos(ModuleName, ultimaID_rama(IDRamaActual)),
	IDPrimeraRama is IDRamaActual+1, %Guardamos el ID de la primera rama colocada.
	
	agregarRamas(ModuleName, C, IDPadre),
	
	datos(ModuleName, ultimaID_rama(IDUltimaRama)), %Guardamos el ID de la última rama colocada.
	
	!, % Como C > 0, borramos toda solución alternativa de 'solve' para mayor eficiencia.
	
	% En el caso de un assert, no podemos invocar directamente A, dado que el aggregate_all ya invoca a todas las alternativas de A.
	% Esto trae problemas notorios en los assert, dado que se invocan dos veces: en el aggregate_all y con A de nuevo.
	% También tener en cuenta que puede ser un predicado built-in que invoque a un assert, modificando el estado de los predicados dinámicos
	% por ello, obtenemos el estado de los predicados dinamicos antes de usar el aggregate y luego después de haberlo usado.
	
	% Debemos necesariamente invocar al predicado A, ya que este podría darle valores a algunas variables, por ejemplo:
	%	call(assertz(p), X is 5)
	% No podríamos en este caso simular C ramas alternativas utilizando un between/3 u otro predicado, dado que no se "incializaría" X.

	
	ModuleName:A,

	
	% buscamos la rama libre que será en la cual se agregará el nodo, si la rama fue podada entonces no es tenida en cuenta.
	buscarRamaLibre(ModuleName, RamaLibre, IDPrimeraRama, IDUltimaRama),

	evaluarCutEnA(ModuleName, A, nodo(IDPadre, IDAbulo, FotPadre, [A | ConjuncionesRestantes])),
	
	agregarNodo(ModuleName, ConjuncionesRestantes, IDPadre, nodo(ID, _, _, _)),

	cambiarHijoRama(ModuleName, RamaLibre, ID),
	aumentarFotogramaActual(ModuleName),
	
	write(A),
	write(" se cumple, pero es built-in, por ello no accedemos."),
	nl.

% En caso de que A esté definida por el usuario y no se satisfaga entonces backtracking.
solve(A, nodo(IDPadre, _, _, _), ModuleName):-
	\+(predicate_property(ModuleName:A, nodebug)),
	
	write("Veremos si va a fallar, del usuario: "),write(A),nl,
	
	\+(clause(ModuleName:A, _)), % si A no se puede satisfacer con nada entonces falla.
	!, % por cuestiones de eficiencia.
	
	agregarNodo(ModuleName, [fail], IDPadre, nodo(ID, _, _, _)),
	agregarRama(ModuleName, IDPadre, ID, _),
	aumentarFotogramaActual(ModuleName),
	
    write("Falla "),
    write(A),
    write(" entonces backtracking."),
    nl,
    fail.
	
% En caso de que A sea provista por el sistema (built-in) y no se satisfaga entonces backtracking.
solve(A, nodo(IDPadre, _, _, _), ModuleName):-
	% A \= (_, _),
	% predicate_property(A, nodebug),
	% \+(A),
	% acá la condición junto al cut no son necesarias, dado a que no hay más alternativas y las condiciones son excluyentes y extensivas (i.e.: abarcan todas las posibilidades)
	
	agregarNodo(ModuleName, [fail], IDPadre, nodo(ID, _, _, _)),
	agregarRama(ModuleName, IDPadre, ID, _),
	aumentarFotogramaActual(ModuleName),
	
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
crearSLD(A, ModuleName, Path):-
	consult(Path),
	
	assertz(datos(ModuleName, fotogramaActual(1))),
	assertz(datos(ModuleName, ultimaID_rama(0))),
	assertz(datos(ModuleName, ultimaID_nodo(0))),
	
	conjuncionesALista(A, Rotulo),
	
	b_setval(ultimoNodoNoDeshecho, nodo(0, -1, 0, Rotulo)),
	vars_to_atom(Rotulo, AtomRotulo),
	assertz(arbol(ModuleName, nodo(0, -1, 0, Rotulo, AtomRotulo))), % Agregamos la raiz del árbol en el fotograma 0.
	
	solve(A, nodo(0, -1, 0, Rotulo), ModuleName).
	% terms_to_atoms_(ModuleName).
	% findall(ElementoArbol, (arbol(ElementoArbol), writeln(ElementoArbol)), Lista).
	
	%unload_file(Path).

% crearSLD(_, M, _):-
	% terms_to_atoms(M).


restablecerPredicadosDinamicos(_, Ant, Ant):- !.

restablecerPredicadosDinamicos(M, Ant, New):-
	writeln('son distintos'),
	forall(member(PN, New), 
	(
		PN = [HN, _],
		retractall(M:HN)
	)),
	

	% tenemos que hacerlo separado, sino volverías estático un hecho y
	% si este tuviese distintas soluciones, no podríamos hacerle retract, ya que sería estático.
	% poner un predicate_property/2 para preguntar si es dinámico no nos va a funcionar, dado que el problema
	% está en que compile_predicates/1 compila predicados que aún no se hicieron retract.

	forall(member(PN, New),
	(	
		PN = [HN, _],
		compile_predicates([M:HN])
	)),
	
	
	
	% Procedemos a recuperar el estado anterior, tener en cuenta que utilizamos assertz, dado a que la lista Ant
	% se encuentra en orden en el que los predicados se hallan.
	
	% Ejemplo:
	%	sea p/1 un predicado dinámico, el cual aparece en memoria con este orden:
	%		p(3), p(0), p(1)
	%	Luego en la lista se guardan en ese mismo orden, por lo que a la hora de recuperarlos,
	%	primero se coloca p(3) al final, luego p(0) al final (tendríamos p(3), p(0)), y por último p(1) 
	%	(quedando p(3), p(0), p(1)).
	

	forall(member(PA, Ant), 
	(
		PA = [HA, BA],
		
		(
			predicate_property(M:HA, static) ->
				dynamic(M:HA)
			;
				true 
		),
		
		assertz(M:(HA :- BA))
	)).
  


conjuncionesALista((A, B), L):-
	!,
	conjuncionesALista(A, As),
	conjuncionesALista(B, Bs),
	append(As, Bs, L).
	
	
conjuncionesALista(A, [A]).

	


eliminarArbol(ModuleName):-
	retractall(arbol(ModuleName, _)),
	retractall(datos(ModuleName, _)).




vars_in_compound_to_atom(X, 1, Y):-
	arg(1, X, V),
	var(V),
	!,
	term_to_atom(V, A),
	copy_term(X, Y),
	arg(1, Y, A).
	
vars_in_compound_to_atom(X, 1, Y):-
	arg(1, X, C),
	compound(C),
	!,
	extract_vars_in_term(C, NC),
	copy_term(X, Y),
	arg(1, Y, NC).
	
vars_in_compound_to_atom(X, 1, Y):-
	arg(1, X, A),
	atomic(A),
	!,
	copy_term(X, Y).
	
vars_in_compound_to_atom(X, IndexX, Y):-
	arg(IndexX, X, A),
	atomic(A),
	!,
	NIndexX is IndexX-1,
	vars_in_compound_to_atom(X, NIndexX, Y).
	
vars_in_compound_to_atom(X, IndexX, Y):-
	arg(IndexX, X, V),
	var(V),
	!,
	term_to_atom(V, A),
	NIndexX is IndexX-1,
	vars_in_compound_to_atom(X, NIndexX, Y),
	arg(IndexX, Y, A).
	
vars_in_compound_to_atom(X, IndexX, Y):-
	arg(IndexX, X, C),
	compound(C),
	extract_vars_in_term(C, NC),
	NIndexX is IndexX-1,
	vars_in_compound_to_atom(X, NIndexX, Y),
	arg(IndexX, Y, NC).
	

extract_vars_in_term(X, X):-
	atomic(X),
	!.	

extract_vars_in_term(X, Y):-
	var(X),
	term_to_atom(X, Y),
	!.
	
extract_vars_in_term(X, Y):-
	compound(X),
	compound_name_arity(X, _, LX),
	vars_in_compound_to_atom(X, LX, Y).
	
	% arg/3, compound/1, var/1, atom/1, compound_name_arguments/3
	% term_to_atom/2 para pasar de var a atom.

vars_to_atom([], []):-
	!.
	
vars_to_atom([X | Xs], [Y | Ys]):-
	% write("Voy a atomizar X: "), write(X), nl,
	extract_vars_in_term(X, Y),
	% write("X atomizado: "), write(X), nl, nl,
	vars_to_atom(Xs, Ys).
