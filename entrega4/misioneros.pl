%problema (“Misioneros”) buscamos la manera mas rapida para tres misioneros y tres
%canibales de cruzar un rio, disponiendo de una canoa que puede ser utilizada
%por 1 o 2 personas (misioneros o canibales), pero siempre evitando que los
%misioneros queden en minoria en cualquier orilla (por razones obvias).

%[numMisionerosIz,numCannIz,numMisionerosDer,numCannDer]


camino(E,E,C,C).
camino(EstadoActual,EstadoFinal,CaminoHastaAhora,CaminoTotal):-
  unPaso(EstadoActual,EstadoSiguiente),
  \+member(EstadoSiguiente,CaminoHastaAhora),
  write([EstadoActual,EstadoSiguiente]),nl,
  camino(EstadoSiguiente,EstadoFinal,[EstadoSiguiente|CaminoHastaAhora],CaminoTotal).

  unPaso(EA,ES) :- nth1(5,EA,Barca), Barca ==0, getItemList4(EA,A,B,C,D),
      esMouMisID([A,B,C,D],[X,Y,Z,K]), ES = [X,Y,Z,K,1].
  unPaso(EA,ES) :- nth1(5,EA,Barca), Barca ==0, getItemList4(EA,A,B,C,D),
      esMouMisID2([A,B,C,D],[X,Y,Z,K]), ES = [X,Y,Z,K,1].
  unPaso(EA,ES) :- nth1(5,EA,Barca), Barca ==0, getItemList4(EA,A,B,C,D),
      esMouCanID([A,B,C,D],[X,Y,Z,K]), ES = [X,Y,Z,K,1].
  unPaso(EA,ES) :- nth1(5,EA,Barca), Barca ==0, getItemList4(EA,A,B,C,D),
      esMouCanID2([A,B,C,D],[X,Y,Z,K]), ES = [X,Y,Z,K,1].
  unPaso(EA,ES) :- nth1(5,EA,Barca), Barca ==0, getItemList4(EA,A,B,C,D),
      esMouCanMisID([A,B,C,D],[X,Y,Z,K]), ES = [X,Y,Z,K,1].
  
  unPaso(EA,ES) :- getItemList4(EA,A,B,C,D),esMouMisDI([A,B,C,D],[X,Y,Z,K]),
      ES = [X,Y,Z,K,0].
  unPaso(EA,ES) :- getItemList4(EA,A,B,C,D),esMouMisDI2([A,B,C,D],[X,Y,Z,K]),
      ES = [X,Y,Z,K,0].
  unPaso(EA,ES) :- getItemList4(EA,A,B,C,D),esMouCanDI([A,B,C,D],[X,Y,Z,K]),
      ES = [X,Y,Z,K,0].
  unPaso(EA,ES) :- getItemList4(EA,A,B,C,D),esMouCanDI2([A,B,C,D],[X,Y,Z,K]),
      ES = [X,Y,Z,K,0].
  unPaso(EA,ES) :- getItemList4(EA,A,B,C,D),esMouCanMisDI([A,B,C,D],[X,Y,Z,K]), 
      ES = [X,Y,Z,K,0].
      
%coge de una lista de 4, cada miembro
getItemList4(List,A,B,C,D) :- nth1(1,List,A),nth1(2,List,B),nth1(3,List,C),
    nth1(4,List,D).
    
% una o dos personas se pueden mover, numMisionerosDer>numCannDer

esMouMisID([A,B,C,D],[A1,B,C1,D]) :- A1 is A-1, A1>=0, C1 is C+1,(A1>=B;A1==0),C1>=D.
esMouMisID2([A,B,C,D],[A1,B,C1,D]) :- A1 is A-2,A1>=0, C1 is C+2,(A1>=B;A1==0),C1>=D.

esMouMisDI([A,B,C,D],[A1,B,C1,D]) :-  A1 is A+1,C1 is C-1,C1>=0,(C1>=D;C1==0),A1>=B.
esMouMisDI2([A,B,C,D],[A1,B,C1,D]) :-  A1 is A+2,C1 is C-2,C1>=0,(C1>=D;C1==0),A1>=B.

esMouCanID([A,B,C,D],[A,B1,C,D1]) :- B1 is B-1,B1>=0,D1 is D+1,(A>=B1;A==0),(C>=D1;C==0).
esMouCanDI([A,B,C,D],[A,B1,C,D1]) :-  B1 is B+1,D1 is D-1,D1>=0,(D1<C;D1==0),(B1<C;C==0).

esMouCanID2([A,B,C,D],[A,B1,C,D1]) :-  B1 is B-2,B1>=0,D1 is D+2,(B1<A;B1==0),(D1<C;C==0).
esMouCanDI2([A,B,C,D],[A,B1,C,D1]) :-  B1 is B+1,D1 is D-1,D1>=0,(D1<C;D1==0),(B1<C;C==0) .

esMouCanMisID([A,B,C,D],[A1,B1,C1,D1]) :- A1 is A-1, A1>0, B1 is B-1, B1>0, (A1>=B1;A1==0),
    C1 is C+1,D1 is D+1,C1>=D1.
esMouCanMisDI([A,B,C,D],[A1,B1,C1,D1]) :- C1 is C-1, C1>0, D1 is D-1, D1>0, (C1>=D1;C1==0),
    A1 is A+1,B1 is B+1,A1>=B1.
nat(0).
nat(N) :- nat(N1),N is N1+1.

nat2(N) :- between(1,3,N).
solucionOptima:-
  nat2(N), % Buscamos solucion de "coste" 0; si no, de 1, etc.
  write(N),nl,
  camino([3,3,0,0,0],[1,1,2,2,1],[[3,3,0,0,0]],C), % Estado = 
			%[numMisionerosIz,numCannIz,numMisionerosDer,numCannDer,posBarca]
  length(C,N), % -el coste es la longitud de C.
  write(C).
 