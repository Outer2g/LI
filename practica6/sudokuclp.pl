:-use_module(library(clpfd)).

% (1) Definir quines variables es faran servir i el seu significat

% (2) Determinar el domini de les variables

% (3) Establir les restriccions entre les variables
  %Xij == valor casella i j
  exemple :- 
           sudoku([5,3,_,_,7,_,_,_,_,
		   6,_,_,1,9,5,_,_,_,
		   _,9,8,_,_,_,_,6,_,
		   8,_,_,_,6,_,_,_,3,
		   4,_,_,8,_,3,_,_,1,
		   7,_,_,_,2,_,_,_,6,
		   _,6,_,_,_,_,2,8,_,
		   _,_,_,4,1,9,_,_,5,
		   _,_,_,_,8,_,_,7,9]).
sudoku(L) :- 
	L = [X11, X12, X13, X14, X15, X16, X17, X18, X19,
	     X21, X22, X23, X24, X25, X26, X27, X28, X29,
	     X31, X32, X33, X34, X35, X36, X37, X38, X39,
	     X41, X42, X43, X44, X45, X46, X47, X48, X49,
	     X51, X52, X53, X54, X55, X56, X57, X58, X59,
	     X61, X62, X63, X64, X65, X66, X67, X68, X69,
	     X71, X72, X73, X74, X75, X76, X77, X78, X79,
	     X81, X82, X83, X84, X85, X86, X87, X88, X89,
	     X91, X92, X93, X94, X95, X96, X97, X98, X99],
	     %determinem domini: Xij 1..9
	L ins 1..9,
	     %restriccions entre variables
	     %files
	all_different([X11,X12,X13,X14,X15,X16,X17,X18,X19]),
	all_different([X21,X22,X23,X24,X25,X26,X27,X28,X29]),
	all_different([X31,X32,X33,X34,X35,X36,X37,X38,X39]),
	all_different([X41,X42,X43,X44,X45,X46,X47,X48,X49]),
	all_different([X51,X52,X53,X54,X55,X56,X57,X58,X59]),
	all_different([X61,X62,X63,X64,X65,X66,X67,X68,X69]),
	all_different([X71,X72,X73,X74,X75,X76,X77,X78,X79]),
	all_different([X81,X82,X83,X84,X85,X86,X87,X88,X89]),
	all_different([X91,X92,X93,X94,X95,X96,X97,X98,X99]),

        % Columnes
	all_different([X11,X21,X31,X41,X51,X61,X71,X81,X91]),
	all_different([X12,X22,X32,X42,X52,X62,X72,X82,X92]),
	all_different([X13,X23,X33,X43,X53,X63,X73,X83,X93]),
	all_different([X14,X24,X34,X44,X54,X64,X74,X84,X94]),
	all_different([X15,X25,X35,X45,X55,X65,X75,X85,X95]),
	all_different([X16,X26,X36,X46,X56,X66,X76,X86,X96]),
	all_different([X17,X27,X37,X47,X57,X67,X77,X87,X97]),
	all_different([X18,X28,X38,X48,X58,X68,X78,X88,X98]),
	all_different([X19,X29,X39,X49,X59,X69,X79,X89,X99]),

        % Quadrats
	all_different([X11,X21,X31,X12,X22,X32,X13,X23,X33]),
	all_different([X14,X24,X34,X15,X25,X35,X16,X26,X36]),
	all_different([X17,X27,X37,X18,X28,X38,X19,X29,X39]),
	all_different([X41,X51,X61,X42,X52,X62,X43,X53,X63]),
	all_different([X44,X54,X64,X45,X55,X65,X46,X56,X66]),
	all_different([X47,X57,X67,X48,X58,X68,X49,X59,X69]),
	all_different([X71,X81,X91,X72,X82,X92,X73,X83,X93]),
	all_different([X74,X84,X94,X75,X85,X95,X76,X86,X96]),
	all_different([X77,X87,X97,X78,X88,X98,X79,X89,X99]),
	     label(L),
	     displaySolSud(L).

displaySolSud(L):- displaySolSud2(L,9).
displaySolSud2([],_).
displaySolSud2(L,0):- L\=[],nl,displaySolSud2(L,9).
displaySolSud2([X|L],N) :- N>0,write(X),write(' '), N1 is N-1, displaySolSud2(L,N1).
%Li == cuantes unitats memporto
china:- L = [A,B,C,D,E,F], 
	    %domini: 0..80 
	    L ins 0..80,
	    %suma tots pesos * numero de items que porto <80
	    A*1+B*2+C*3+D*5+E*6+F*7 #< 80,
	    %label priorizando el valor mayor
	    labeling([max(1*A+4*B+7*C+11*D+14*E+15*F)],L),
	    write(L).

sendMore:- L = [S,E,N,D,M,O,R,Y],
	    L ins 1..9,
	    S+E+N+D+M+O+R+E #= M+O+N+E+Y,
	    label(L),
	    write(L).
	    
task(1,8).
task(2,6).
task(3,7).
task(4,5).
task(5,2).
task(6,3).
task(7,8).
task(8,6).
task(9,2).
task(10,6).
task(11,1).
task(12,2).
task(13,6).
task(14,4).

sumaT(_,[],_,0).
sumaT(Ord,[X|T],Tarea,S) :- Ord == X,NovaT is Tarea+1, sumaT(Ord,T,NovaT,S1), 
			    task(Tarea,A), S is S1+A,!.
sumaT(Ord,[_|T],Tarea,S) :- NT is Tarea+1, sumaT(Ord,T,NT,S).

%ordenador i hace tareas : 1..14
computa:- T = [A,B,C,D,E,F,G,H,I,J,K,L,M,N],
	  T ins 1..5,
	  label(T),
	  write(T),nl,
	  L = [O,P,Q,R,S],
	  sumaT(1,T,1,Suma1),
	  sumaT(2,T,1,Suma2),
	  sumaT(3,T,1,Suma3),
	  sumaT(4,T,1,Suma4),
	  sumaT(5,T,1,Suma5),
	  
	  O #= Suma1,
	  P #= Suma2,
	  Q #= Suma3,
	  R #= Suma4,
	  S #= Suma5,
	  
	  O #=< P,
	  P #=< Q,
	  Q #=< R,
	  R #=< S,
	  label(L),
	  write(O),halt.