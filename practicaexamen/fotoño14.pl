:-use_module(library(clpfd)).

solution:-
  L =[Jonh,Paul,Ringo],
  L ins 1..10,
  Jonh #>= 2*Ringo,
  Paul #>= 3+Jonh,
  Ringo #>= 3,
  label(L),
  write(L),nl,
  fail.
solution.