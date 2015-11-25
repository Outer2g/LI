perm([],[]).
 perm([X|L],P):- perm(L,P), append(A,B,P1),
		  append(A,[X|B],P).
