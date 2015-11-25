:-include(entradaFlow9).
:-include(displayFlow).
:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

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
%================================================================
%Numero variables: 3
%Variable 1: col-I-J-C : Casella IJ te color K
%Variable 2: s-X-Y-A-B : Successor de casella XY es la casella AB
%Variable 3: x-I-J-K : Casella IJ te el numero K. Aquesta variable esta
%		pensada per tal devitar cicles entre succesors

%========== Utils especifics =============

col(C) :- c(C,_,_,_,_).

notFinal(I,J) :- c(_,_,_,I,J),!,fail.
notFinal(_,_).

notInitial(I,J):- c(_,I,J,_,_),!,fail.
notInitial(_,_).

isInitial(I,J) :- c(_,I,J,_,_).
isFinal(I,J) :- c(_,_,_,I,J).

diferent(I,J,I,J):- !, fail. 
diferent(_,_,_,_).

%retorna el id de les caselles adjacents a IJ
volt(I,J,A,B):- size(N),A is I+1, B is J, A<N+1.
volt(I,J,A,B):- size(N),A is I, B is J+1,B<N+1.
volt(I,J,A,B):- A is I-1,B is J,A>0.
volt(I,J,A,B):- A is I, B is J-1,B>0.
% ========= Start ====================
%Functions:
writeClauses:- 
  placeInitials,
  exactlyOneColorPerCas,
  exactlyOneSucc,
  succSameColorAnt,
  finalNoSucc,
  atMostOneAnt,
  exactlyOneNumPerCas,
  avoidCicles,
  notDuplicatedNums.
  
%%%%%%%%%%%

succSameColorAnt :- square(I,J), notFinal(I,J),volt(I,J,A,B),
    col(C),writeClause([\+col-I-J-C, \+s-I-J-A-B,col-A-B-C]),fail.
succSameColorAnt.
placeInitials:- c(C,I,J,A,B), writeClause([col-I-J-C]),writeClause([col-A-B-C]),
    fail.
placeInitials.

exactlyOneColorPerCas :- square(I,J), findall(col-I-J-C,col(C),Lits)
    ,exactlyOne(Lits),fail.
exactlyOneColorPerCas.

finalNoSucc :- square(I,J), isInitial(I,J),volt(I,J,A,B),
    writeClause([\+s-A-B-I-J]),fail.
finalNoSucc.

atMostOneAnt :- square(I,J),notInitial(I,J),
    findall(s-A-B-I-J,volt(I,J,A,B),Lits),atMostOne(Lits),fail.
atMostOneAnt.

exactlyOneNumPerCas :- size(N),square(I,J),X is N*N,
    findall(x-I-J-K,value(X,K),Lits),exactlyOne(Lits),fail.
exactlyOneNumPerCas.

notDuplicatedNums :- size(N),square(I,J),
    square(A,B),X is N*N,value(X,K),diferent(I,J,A,B),
    writeClause([\+x-I-J-K,\+x-A-B-K]),fail.
notDuplicatedNums.

avoidCicles :- size(N),square(I,J),volt(I,J,A,B),X is N*N+1,value(X,L),
    K is L+1, K<X,writeClause([\+s-I-J-A-B,\+x-I-J-L,x-A-B-K]),fail.
avoidCicles.

exactlyOneSucc :- square(I,J),notFinal(I,J),
    findall(s-I-J-A-B,volt(I,J,A,B),Lits),exactlyOne(Lits),fail.
exactlyOneSucc.



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
