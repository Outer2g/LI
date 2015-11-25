 
camino (E,E,C,C).
camino (EstadoActual,EstadoFinal,CaminoHastaAhora,CaminoTotal):-
  unPaso(EstadoActual,EstadoSiguiente),
  \+member(EstadoSiguiente,CaminoHastaAhora),
  camino(EstadoSiguiente,EstadoFinal,[EstadoSiguiente|CaminoHastaAhora],CaminoTotal).

%coge de una lista de 4, cada miembro
getItemList4(List,A,B,C,D) :- nth1(1,List,A),nth1(2,List,B),nth1(3,List,C),
    nth1(4,List,D).
    
solucionOptima:-
  nat(N), % Buscamos solucion de "coste" 0; si no, de 1, etc.
  camino([3,3,0,0],[0,0,3,3],[[3,3,0,0]],C), % En "hacer aguas": -un estado es [cubo5,cubo8], y
  length(C,N), % -el coste es la longitud de C.
  write(C).
 