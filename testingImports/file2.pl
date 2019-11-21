q(ModuleName, Path):-
	concat(Path, ModuleName, PathAux),
	concat(PathAux, '.pl', PathModule),
	consult(PathModule), 
	ModuleName:p,
	k(ModuleName).

k(ModuleName):-
	ModuleName:t.