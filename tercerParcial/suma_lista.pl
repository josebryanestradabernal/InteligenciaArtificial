suma_aux(_,[],R):-R=[].
suma_aux(A,L,R):- [X|T]=L, B is X+A,suma_aux(B,T,C),R=[B|C].
suma_anteriores(L,R):- [X|T]=L,suma_aux(X,T,C),R=[X|C].


% Ejercicio de arbol genealogico
% Definicion de la base de conocimiento
padre(bryan,bill).
padre(bryan,david).
padre(david,alexis).
padre(david,kevin).
padre(bill,kely).
padre(bill,valeria).
padre(valeria,vaneza).
padre(vaneza,brandon).
padre(vaniza,noel).

hermano(X,Y):-padre(Z,X),padre(Z,Y), not(X==Y).
nieto(X,Y):- padre(Y,Z),padre(Z,X).
primo(X,Y):- padre(Z,Y),padre(W,X),hermano(Z,W).
descendiente(X,Y):- padre(Y,X).
descendiente(X,Y):- padre(Z,X),descendiente(Z,Y).
%descendiente(X,Y) :- padre(Y,X).
%descendiente(X,Y) :-padre(Y,Z), descendiente(X,Z).



% Ejercicio de operaciones con conjuntos

list([]).
list([_|_]).
   
lt(X,Y):-var(X);var(Y).
lt(X,Y):-nonvar(X),nonvar(Y),X<Y.


elemento_de(X,[X|T]).
elemento_de(X,[Y|T]):-lt(Y,X),in(X,T).

cardinalidad_de(L,N):-length(L, N).

subconjunto_de([],V).
subconjunto_de([H|T1],[H|T2]) :- subconjunto_de(T1,T2).
subconjunto_de([H1|T1],[H2|T2]) :-   lt(H2,H1),subconjunto_de([H1|T1],T2).

union_de([],S,S).
union_de(S,[],S):-S\=[].
union_de([X|TX],[X|TY],[X|TZ]):-
   union_de(TX,TY,TZ).
union_de([X|TX],[Y|TY],[X|TZ]):-
   lt(X,Y),
   union_de(TX,[Y|TY],TZ).
union_de([X|TX],[Y|TY],[Y|TZ]):-
   lt(Y,X),
   union_de([X|TX],TY,TZ).
   
intersection_de([],S,[]).
intersection_de(S,[],[]):-S\=[].
intersection_de([X|TX],[X|TY],[X|TZ]):-
   intersection_de(TX,TY,TZ).
intersection_de([X|TX],[Y|TY],TZ):-
   lt(X,Y),
   intersection_de(TX,[Y|TY],TZ).
intersection_de([X|TX],[Y|TY],TZ):-
   lt(Y,X),
   intersection_de([X|TX],TY,TZ).
   
diferencia_de([],S,[]).
diferencia_de(S,[],S):-S\=[].
diferencia_de([X|TX],[X|TY],TZ):-
   diferencia_de(TX,TY,TZ).
diferencia_de([X|TX],[Y|TY],[X|TZ]):-
   lt(X,Y),
   diferencia_de(TX,[Y|TY],TZ).
diferencia_de([X|TX],[Y|TY],TZ):-
   lt(Y,X),
   diferencia_de([X|TX],TY,TZ).