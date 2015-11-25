:-include(sud22).
:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.
%Constants
size(N) :- N is 9.
% ========= Utils Generics ======================
value(N,K) :- between(1,N,K).
square(I,J) :- size(N),value(N,I),value(N,J).
square(N,I,J) :- value(N,I),value(N,J).
atLeastOne(Lits) :- writeClause(Lits).
atLeastOne(_).
atMostOne(Lits) :- append(_,[X|Lits1],Lits), append(_,[Y|_],Lits1),
    negate(X,NX),negate(Y,NY), writeClause([NX,NY]),fail.
atMostOne(_).
negate(\+X,X):-!.
negate(X,\+X). 
exactlyOne(Lits) :- atLeastOne(Lits),atMostOne(Lits).
%====================START==========================
writeClauses:- exactlyOnePerCas,atMostOnePerRow,atMostOnePerColumn,
	block,fill.
	
fill:- filled(I,J,K), writeClause( [ x-I-J-K ] ), fail.
fill.

block :- between(1,3,C),between(1,3,P), A1 is C*3-2, A2 is C*3, 
	B1 is P*3-2, B2 is P*3, between(A1,A2,I), between(B1,B2,J),
	between(1,9,K), between(A1,A2,I2), between(B1,B2,J2), I2 > I,
	writeClause([\+x-I-J-K,\+x-I2-J2-K]), fail.
block.
		

exactlyOnePerCas :- square(I,J),findall(x-I-J-K,value(9,K),Lits),
    exactlyOne(Lits),fail.
exactlyOnePerCas.

atMostOnePerColumn :- square(J,K),findall(x-I-J-K,value(9,I),Lits),
    atMostOne(Lits),fail.
atMostOnePerColumn.

atMostOnePerRow :- square(I,K),findall(x-I-J-K,value(9,J),Lits),
    atMostOne(Lits),fail.
atMostOnePerRow.


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
