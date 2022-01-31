
%===============================
% 02/06/2021
% Estrada Bernal José Bryan
%===============================


% Ejercicio de suma_anteriores
suma_aux(_,[],R):-R=[].
suma_aux(A,L,R):- [X|T]=L, B is X+A,suma_aux(B,T,C),R=[B|C].
suma_anteriores(L,R):- [X|T]=L,suma_aux(X,T,C),R=[X|C].

% ======================================
% Para esta parta se debe cargar la 
% base de conocimiento
% consult(BaseConocimiento).
%=======================================
% Ejercicio de arbol genealogico


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
   
interseccion_de([],S,[]).
interseccion_de(S,[],[]):-S\=[].
interseccion_de([X|TX],[X|TY],[X|TZ]):-
   interseccion_de(TX,TY,TZ).
interseccion_de([X|TX],[Y|TY],TZ):-
   lt(X,Y),
   interseccion_de(TX,[Y|TY],TZ).
interseccion_de([X|TX],[Y|TY],TZ):-
   lt(Y,X),
   interseccion_de([X|TX],TY,TZ).
   
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



connected(X, Y) :-sigue(X, Y, _) ;  sigue(Y, X, _).

%====Retorna todas las rutas posibles para llegar de A a B

rutas(A,B,Path) :-
       travel(A,B,[A],Q), 
       reverse(Q,Path).

pathAux(A,B,Path,Len) :-
       travel(A,B,[A],Q), 
       reverse(Q,Path),length(Path, Len).

travel(A,B,P,[B|P]) :- 
       connected(A,B).
travel(A,B,Visited,Path) :-
       connected(A,C),           
       C \== B,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path). 

%====Definicion de la ruta mas corta ========

ruta_mas_corta(A, B,Path1) :-
    findall(p(Len, Path),
            pathAux(A, B, Path, Len),
            Paths),
    sort(Paths, Sorted),
    Sorted = [p(Len1, Path1) | _].

