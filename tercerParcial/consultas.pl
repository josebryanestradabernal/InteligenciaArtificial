?-suma_anteriores([1,2,3,4,5],R).
%R= [1,3,6,10,15].
?-padre(bryan,bill). 
% true
?-hermano(valeria,kely).
%true
?-nieto(valeria,bill).
%false valeria es nieta de bryan
?-primo(alexis,kely).
%true
?-descendiente(alexis,bryan).
%true
?-elemento_de(2,[1,2,3,4]).
%true
?-cardinalidad_de([1,2,3,4],N).
%N=4
?-subconjunto_de([1,2,3],[1,2,3,4]).
%true
?-union_de([12,3],[1,2,3,4],R).
%R=[12,3,1,2,4]
?-intersection_de([12,3],[1,2,3,4],R).
%R= [3]
?-diferencia_de([12,3],[1,2,3,4],R).
%R = [12]
?-rutas(constituyentes,polanco,Ruta).
%Ruta da aproximadamente 1000 resultados diferentes
?-ruta_mas_corta(constituyentes,polanco,Ruta).
%Ruta = [constituyentes, auditorio, polanco]

