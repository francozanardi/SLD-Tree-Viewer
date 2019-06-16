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
	
q(X):-
	writeln("es mayor a 0"),
	true,
	fail.
	
q(X).
	
r(X):-
	Lista = [X],
	length(Lista, R),
	append(Lista, Lista, NL),
	length(NL, R2).

% La idea es hacer un meta-intérprete capaz de realizar un árbol SLD para un programa P.
% Se hacen uso de assertz y retract para no verse afectado por el backtracking del meta-intérprete y conservar todo nodo,
% de esta manera podremos ver los nodos que llevaron a caminos fallidos y como fue el árbol completo.
% Además no se acceden a predicados predefinidos por el sistema (built-in) y solo se evalúa la veracidad de estos.
% crearSLD/2 es el predicado encargado de crear el árbol para una consulta ingreada en su primer argumento.



% En este caso tenemos una conjunción de reglas (o hechos), por lo tanto accesamos a cada uno.
solve((A, B), NodoPadre):-
    solve(A, NodoPadre),
	
	% Aquí tomamos el nodo con mayor ID ya que, A podría haber sido una regla y tomar muchos nodos para solucionarse.
	% Si se está aquí entonces A se resolvió, y el nodo con ID mayor es quien lo resolvió, entonces este nodo será el padre 
	% de las conjunciones restantes.
	getMaxID(UltimaID),
	nodoArbol(nodo(UltimaID, Conj, IDP)),
    solve(B, nodo(UltimaID, Conj, IDP)).




% A es una regla definida por el usuario, donde puede que después de ella se tengan que resolver más reglas que están en conjunción.
% Por ello al rotulo de su padre, debe quitarse ella y reemplazarse por su cuerpo.
solve(A, nodo(IDPadre, [A | ConjuncionesRestantes], _)):-

    % A \= true,
    A \= (_, _),
	% Verificamos que el predicado no sea uno provisto por el sistema (built_in), en caso de serlo es privado y no podemos acceder a su cuerpo
	not(predicate_property(A, built_in)), 
    clause(A, B),
  	B \= true,
	
	% Acá buscamos la UltimaID agregada y no hacemos IDPadre+1, esto es porque si hay backtracking porque falla alguna conjunción restante
	% y además de eso hay otras reglas que pueden instanciarse de distinta manera, lo que implicaría que aquí podríamos tomar otro camino.
	% Por eso mismo nosotros comenzamos nuestro camino con una ID+1 de la última ID que falló.
	% Notar que nuestro padre será IDPadre, ya que por tomar otro camino no deberíamos cambiar de padre.
	getMaxID(UltimaID),
	ID is UltimaID + 1,
	conjuncionesALista(B, BResultado),
	append(BResultado, ConjuncionesRestantes, Rotulo),
	assertz(nodoArbol(nodo(ID, Rotulo, IDPadre))),
	
    write(A),
    write(" es la cabeza de una regla con cuerpo "),
    write(B),
    nl,
	
    solve(B, nodo(ID, Rotulo, IDPadre)).

% A es un hecho definido por el usuario y se satisface, por lo tanto el nuevo rótulo será el del padre sin A.
solve(A, nodo(IDPadre, [A | ConjuncionesRestantes], _)):-
    % A \= true,
	
    A \= (_, _),
	not(predicate_property(A, built_in)), % verificamos que no sea algo provisto por el sistema
    clause(A, true),
	
	% Acá buscamos la UltimaID agregada y no hacemos IDPadre+1, esto es porque si hay backtracking porque falla alguna conjuncion restante
	% y además de eso hay otros hechos que pueden instanciarse de distinta manera, entonces podemos aquí tomar otro camino.
	% Notar que nuestro padre será IDPadre, ya que por tomar otro camino no deberíamos cambiar de padre.
	getMaxID(UltimaID),
	ID is UltimaID + 1,
	assertz(nodoArbol(nodo(ID, ConjuncionesRestantes, IDPadre))),
	
    write(A),
    write(" es la cabeza de un hecho."),
    nl.


% En caso de que el predicado sea provisto por el sistema no podemos acceder a su cuerpo, por ello solo lo colocamos en el rótulo sin accederlo.
solve(A, nodo(IDPadre, [A | ConjuncionesRestantes], _)):-
	A \= (_, _),
	predicate_property(A, built_in),
	A, %simplemente invocamos A para ver si se satisface.
	
	ID is IDPadre + 1,
	assertz(nodoArbol(nodo(ID, ConjuncionesRestantes, IDPadre))),
	
    write(A),
    write(" es provisto por el sistema, por lo tanto no lo accesamos."),
    nl.

% En caso de que A esté definida por el usuario y no se satisfaga entonces backtracking.
solve(A, nodo(IDPadre, _, _)):-
	A \= (_, _),
	not(predicate_property(A, built_in)),
	not(clause(A, _)), % si A no se puede satisfacer con nada entonces falla.
	
	ID is IDPadre + 1,
	assertz(nodoArbol(nodo(ID, [fail], IDPadre))),
	
    write("Falla "),
    write(A),
    write(" entonces backtracking."),
    nl,
    fail.
	
% En caso de que A sea provista por el sistema (built-in) y no se satisfaga entonces backtracking.
solve(A, nodo(IDPadre, _, _)):-
	A \= (_, _),
	predicate_property(A, built_in),
	not(A),
	
	ID is IDPadre + 1,
	assertz(nodoArbol(nodo(ID, [fail], IDPadre))),
	
    write("Falla "),
    write(A),
    write(" built-in, entonces backtracking."),
    nl,
    fail.


crearSLD(A, Lista):-
	A \= (_, _),
	assertz(nodoArbol(nodo(0, [A], -1))),
	solve(A, nodo(0, [A], -1)),
	findall(Nodo, (nodoArbol(Nodo), writeln(Nodo)), Lista).



conjuncionesALista(Elemento, [Elemento]):-
	Elemento \= (_, _).

conjuncionesALista((X, Xs), [X | Lista]):-
	conjuncionesALista(Xs, Lista).

% getMaxID(-MaxID)	
getMaxID(MaxID):-
	nodoArbol(nodo(MaxID, _, _)),
	esMayor(nodo(MaxID, _, _)).
	
esMayor(nodo(ID, _, _)):-
	forall( (nodoArbol(nodo(IDaux, _, _)), ID \= IDaux), IDaux < ID). %Las IDs son únicas.
	
eliminarNodos:-
	forall(nodoArbol(N), retract(nodoArbol(N))).