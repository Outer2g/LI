 
camino(E,E,C,C).
camino(EstadoActual,EstadoFinal,CaminoHastaAhora,CaminoTotal):-
  unPaso(EstadoActual,EstadoSiguiente),
  \+member(EstadoSiguiente,CaminoHastaAhora),
  camino(EstadoSiguiente,EstadoFinal,[EstadoSiguiente|CaminoHastaAhora],CaminoTotal).

  p1(1).
  p2(2).
  p5(5).
  p8(8).
  
  unPaso([0,0,C,D,0,S],[1,1,C,D,1,S1]) :- p2(X),S1 is S + X.
  unPaso([0,B,0,D,0,S],[1,B,1,D,1,S1]) :- p5(X),S1 is S + X.
  unPaso([0,B,C,0,0,S],[1,B,C,1,1,S1]) :- p8(X),S1 is S + X.
  
  
  unPaso([A,0,0,D,0,S],[A,1,1,D,1,S1]) :- p5(X),S1 is S + X.
  unPaso([A,0,C,0,0,S],[A,1,C,1,1,S1]) :- p8(X),S1 is S + X.
  
  
  unPaso([A,B,0,0,0,S],[A,B,1,1,1,S1]) :- p8(X),S1 is S + X.
  

  unPaso([1,B,C,D,1,S],[0,B,C,D,0,S1]) :- p1(X),S1 is S + X.
  unPaso([A,1,C,D,1,S],[A,0,C,D,0,S1]) :- p2(X),S1 is S + X.
  unPaso([A,B,1,D,1,S],[A,B,0,D,0,S1]) :- p5(X),S1 is S + X.
  unPaso([A,B,C,1,1,S],[A,B,C,0,0,S1]) :- p8(X),S1 is S + X.
  
  
%coge de una lista de 4, cada miembro
getItemList4(List,A,B,C,D) :- nth1(1,List,A),nth1(2,List,B),nth1(3,List,C),
    nth1(4,List,D).
% [p1,p2,p3,p4,posLinterna,tiempoInvertido]

nat2(N) :- between(1,2,N).

nat(0).
nat(N):- nat(N1), N is N1+1. 
solucionOptima:-
    nat(N),
    camino([0,0,0,0,0,0],[1,1,1,1,1,N],[[0,0,0,0,0,0]],C),
    write(C).
 