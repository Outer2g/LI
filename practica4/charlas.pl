solucionOptima:-
datosEjemplo([[X,Y,Z]|L]),
nat(N),
sol(N,L,[[X],[Y],[Z]],S),
writeSol(S).

nat(0).
nat(N):-nat(N1),N is N1+1.

sol(_,[],S,S).
sol(N,[[X,Y,Z]|L],S1,S2):- possible([X,Y,Z],S1,S3),
  cost(S3,K),K=<N,sol(N,L,S3,S2).
  

writeSol([[],[],[]]).
writeSol([L1,L2,L3]):-pertConResto(X,L1,L11), qp(X,L1), nl,write('- '), write(X), 
  pertConResto(Y,L2,L22), qp(Y,L2), write(' '), write(Y), 
  pertConResto(Z,L3,L33), qp(Z,L3), write(' '), write(Z), 
  writeSol([L11,L22,L33]).
  
  qp(' ',[]).
  qp(X,[X|_]).
  
cost([],0).
cost([X|L],C):- cost(L,C1), length(X,C2), C is C1 + C2. 

possible([X,Y,Z],[S1,S2,S3],[S4,S5,S6]):-
  permutation([X,Y,Z],[A,B,C]), add(A,S1,S4),
  add(B,S2,S5), add(C,S3,S6).
  
add(X,L,L) :- member(X,L),!.
add(X,L,L2) :- append(L,[X],L2).

pertConResto(' ',[],[]).
pertConResto(X,L,R) :- append(L1,[X|L2],L),append(L1,L2,R).

datosEjemplo([[1,2,6],[1,6,7],[2,3,8],[6,7,9],[6,8,9],[1,2,4],[3,5,6],[3,5,7],
               [5,6,8],[1,6,8],[4,7,9],[4,6,9],[1,4,6],[3,6,9],[2,3,5],[1,4,5],
               [1,6,7],[6,7,8],[1,2,4],[1,5,7],[2,5,6],[2,3,5],[5,7,9],[1,6,8]]).