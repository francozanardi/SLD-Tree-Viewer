a:- b, d.
a:- c, b.
d:- c, e.
%e:- fail. para hacerlo funcionar tengo que en la consola hacer assertz(e) y después retract(e).
c.
b.


p(X, Y):-
	Y = 0,
	X > 0,
	q(X).
	
p(X, Y):-
	\+(X > 0),
	r(X).

p(X, Y). %Alternativa que siempre se cumple
	
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

% este programa trae problemas con los cuts, no podemos reconocer particularmente a cada uno.
% Es decir, nuestro problema es que debemos identificar a los cuts con ID's separados.
bug:-
	bugAux, !.
bug.

bugAux:-
	!, fail.