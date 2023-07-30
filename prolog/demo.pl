loves(romeo, juliet).

loves(juliet, romeo) :- loves(romeo, juliet).

male(albert).
male(bob).

female(alice).

happy(albert).
happy(alice).
happy(bill).
with_albert(alice).

runs(albert) :-
  happy(albert).
  
dances(alice) :-
  happy(alice),
  with_albert(alice).
  
does_alice_dance :- dances(alice),
  write('When Alice is happy and with Albert she dances').
  
swims(bob) :-
  happy(bob),
  near_water(bob).
  
swims(bill) :- happy(bill).

swims(bill) :- near_water(bill).

parent(albert, bob).
parent(alice, bob).