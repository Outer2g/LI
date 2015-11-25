:-include(sud22).
:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

writeClauses:- atLeastOneNumberPerCas, atMostOneNumberPerCas,atMostOnePerRow,atMostOnePerColumn,
	atMostOnePerBox.
		
		
atMostOnePerBox :- between(1,3,Y),between(1,3,Z),P1 is Y*3,P2 is Z*3,atMostOnePerBoxAux(P1,P2),fail.
atMostOnePerBox.

atMostOnePerBoxAux(Y,Z):- N1 is Y-2,N2 is Z-2,between(1,9,K),F1 is N1,F2 is N2+1, F3 is Y,
	C1 is N2, C2 is N2+1, C3 is Z,
	writeClause([\x+F1-C1-K,\x+F1-C2-K,\x+F1-C3-K,
		     \x+F2-C1-K,\x+F2-C2-K,\x+F2-C3-K,
		     \x+F3-C1-K,\x+F3-C2-K,\x+F3-C3-K]),fail.
atMostOnePerBoxAux(_,_).

atMostOnePerColumn:- N is 9,between(1,N,J),between(1,N,K),between(1,N,I1),between(1,N,I2),I1<I2,
	writeClause([\+x-I1-J-K,\+x-I2-J-K]),fail.
atMostOnePerColumn.

atMostOnePerRow :- N is 9, between(1,N,I),between(1,N,K), between(1,N,J1),between(1,N,J2),J1<J2,
	writeClause([\+x-I-J1-K,\+x-I-J2-K]),fail.
atMostOnePerRow.

atLeastOneNumberPerCas:- N is 9, between(1,N,I), between(1,N,J),
	findall(x-I-J-K,between(1,N,K),C),writeClause(C),fail.
atLeastOneNumberPerCas.

atMostOneNumberPerCas:- N is 9, between(1,N,I),between(1,N,J),between(1,N,K1),between(1,N,K2),K1<K2,
	writeClause([\+x-I-J-K1,\+x-I-J-K2]),fail.
atMostOneNumberPerCas.


%displaySol([]).
%displaySol([Nv|S]):- num2var(Nv,x-I-J-K), write(x-I-J-K),write(','),displaySol(S).

displaySol(L):-
  between(1,9,I),nl,
  between(1,9,J), between(1,9,K),
  var2num(x-I-J-K,N),
  member(N,L), write(K), write(' '),fail.
displaySol(_).

% ========== No need to change the following: =====================================

main:- symbolicOutput(1), !, writeClauses, halt. % escribir bonito, no ejecutar
main:-  assert(numClauses(0)), assert(numVars(0)),
	tell(clauses), writeClauses, told,
	tell(header),  writeHeader,  told,
	unix('cat header clauses > infile.cnf'),
	unix('picosat -v -o model infile.cnf'),
	unix('cat model'),
	see(model), readModel(M), seen, displaySol(M),
	halt.

var2num(T,N):- hash_term(T,Key), varNumber(Key,T,N),!.
var2num(T,N):- retract(numVars(N0)), N is N0+1, assert(numVars(N)), hash_term(T,Key),
	assert(varNumber(Key,T,N)), assert( num2var(N,T) ), !.

writeHeader:- numVars(N),numClauses(C),write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-  retract(numClauses(N)), N1 is N+1, assert(numClauses(N1)),!.
writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w( Lit ):- symbolicOutput(1), write(Lit), write(' '),!.
w(\+Var):- var2num(Var,N), write(-), write(N), write(' '),!.
w(  Var):- var2num(Var,N),           write(N), write(' '),!.
unix(Comando):-shell(Comando),!.
unix(_).

readModel(L):- get_code(Char), readWord(Char,W), readModel(L1), addIfPositiveInt(W,L1,L),!.
readModel([]).

addIfPositiveInt(W,L,[N|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, !.
addIfPositiveInt(_,L,L).

readWord(99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!.
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
