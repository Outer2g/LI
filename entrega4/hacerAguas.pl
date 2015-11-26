camino(E,E,C,C).
camino(EstadoActual,EstadoFinal,CaminoHastaAhora,CaminoTotal):-
  unPaso(EstadoActual,EstadoSiguiente),
  \+member(EstadoSiguiente,CaminoHastaAhora),
  camino(EstadoSiguiente,EstadoFinal,[EstadoSiguiente|CaminoHastaAhora],CaminoTotal).

  getCubos(EA,C5,C8) :- nth1(1,EA,C5),nth1(2,EA,C8).
unPaso(EA,ES) :- getCubos(EA,C5,_), C5==0, ompleA(EA,ES).
unPaso(EA,ES) :- getCubos(EA,C5,_), C5>0, verteixA(EA,ES).
unPaso(EA,ES) :- getCubos(EA,_,C8), C8==8, vaciaB(EA,ES).

vaciaA([_|Y],[0|Y]).
vaciaB([X|_],[X,0]).
ompleA([_|Y],[5|Y]).
ompleB([X|_],[X,8]).
verteixA([X|Y],[X1,Y1]) :- X1 is 0,Y1 is X+Y,Y1<9,!.
verteixA([X|Y],[X1,Y1]) :- X1 is X-(8-Y),Y1 is 8.
verteixB([X|Y],[X1,Y1]) :- X1 is X+Y, Y1 is 0, X1<6,!.
verteixB([X|Y],[X1,Y1]) :- X1 is 5, Y1 is Y-(5-X).

%una sol: llena 5,vierte 5, llena 5, vierte 5, vacia 8,vierte 5,
%=llena 5,vierte 5, llena 5,vierte 5,vacia 8, vierte 5.

nat(N) :- between(1,50,N).
solucionOptima:-
  nat(N), % Buscamos solucion de "coste" 0; si no, de 1, etc.
  
  camino([0,0],[0,4],[[0,0]],C), % En "hacer aguas": -un estado es [cubo5,cubo8], y
  
  length(C,N), % -el coste es la longitud de C.
  write(C).