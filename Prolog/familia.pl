 hermano(juan,pedro).
 hermano(enrique,pablo).
 
 padre(roberto,juan).
 padre(marta,enrique).
 
 pert(X,[X|_]).
 pert(X,[_|L]) :- pert(X,L).
 
 concat([],L,L).
 concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).
 
 fact(0,1):-!.
 fact(N,F) :- N1 is N-1, fact(N1,F1), F is F1*N.
 
 d(X,X,1):-!.
 d(c,X,0):- atomic(c).
 d(A+B,X,U+V) :- d(A,X,U),d(B,X,V).
 d(A*B,X,U*B+A*V):- d(A,X,U),d(B,X,V).
 
 derysimpl(E1,X,E2):- d(E1,X,E3),simplif(E3,E2).
 
 simplif(E1,E2):- unpaso(E1,E3),simplif(E3,E2).
 simplif(E,E).
 
 unpaso(E1,E2):- S(E1,E2).
 unpaso(E1+E2,E3+E2):- unpaso(E1,E3).
 unpaso(E2+E1,E2+E3):- unpaso(E1,E3).
 
 S(E+0,E).
 S(0+E,E).
 S(1*E,E).
 S(0*E,0).
 
 subset([],[]).
 subset([X|L],[X|S]):- subset(L,S).
 subset([X|L],S):-subset(L,S).
 
 perm([],[]).
 perm([X|L],P):- perm(L,P), append(A,B,P1),
		  append(A,[X|B],P).
		  
 cifras(L,N):- subset(L,S),perm(S,P),expr(P,E),B is E,write (E).
 
 expr([X],X).:-!.
 expr(L,E1+E2):- L1\=[],L2\=[],append(L1,L2,L),expr(L1,E1),expr(L2,E2).
 expr(L,E1*E2):- L1\=[],L2\=[],append(L1,L2,L),expr(L1,E1),expr(L2,E2).
 expr(L,E1\E2):- L1\=[],L2\=[],append(L1,L2,L),expr(L1,E1),expr(L2,E2),R is E2,R\=0.
 expr(L,E1-E2):- L1\=[],L2\=[],append(L1,L2,L),expr(L1,E1),expr(L2,E2).

 nat(0).
 nat(N):- nat (N1),B is N1+1.
 
 tio(S,T):- padre(S,P), hermano(P,T).