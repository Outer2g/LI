% cada estudiante selecciona una charla en un bloque [A,B,C], podemos juntarlos con tal que 
% las charlas dadas son minimas?
%entrada: [A,B,C]X24
%Salida: [[],[],[]] lista 1: grupos que daran la charla en ese slot
%idea: con que un estudiante quiera la charla, la ponemos

solucionOptima(Opciones,[A,B,C]) :- considerarSlot1(Opciones,A),considerarSlot2(Opciones,B),
  considerarSlot3(Opciones,C).
  
  sizeE(N):- between(1,24,N).
  addC(E,L,[E|L]).
considerarSlot1(E,A) :- sizeE(N),Y=[],nth1(N,E,C),nth1(1,C,F),
  \+member(F,Y),addC(F,Y,A).