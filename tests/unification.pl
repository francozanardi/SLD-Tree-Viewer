p(X):-
	q(X).

q(X):-
	w(X).
	
w(0).


crear(A):-
	solve(A).
	
solve((A, B)):-
	!,
	solve(A),
	solve(B).
	
solve(A):-
	clause(A, true),
	!,
	assertz(pred(A)).
	
solve(A):-
	clause(A, B),
	assertz(pred(A)),
	solve(B).
	
	
	
% ENCONTRADO! EL PROBLEMA ESTÁ A LA HORA DE QUERER RECUPERAR LOS VALORES, 
% POR CADA CONSULTA, PROLOG NOS CAMBIA LA REPRESENTACION INTERNA.
% SI LO HACEMOS TODO EN UNA CONSULTA, PODREMOS OBTENER EL VALOR QUE CORRESPONDA.