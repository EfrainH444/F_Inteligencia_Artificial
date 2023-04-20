

/*
%==============================================================================================================%

         		              EFRAIN CHAVEZ HERNANDEZ
       			       FUNDAMENTOS DE INTELIGENCIA ARTIFICIAL
        		            
          Resuelva en Prolog  los tres problemas básicos de unificación que se encuentran al 
	       	          final de la presentación #16 (versiones recortadas:)
	       	          
		  			    VERSION 1

  PREDICADOS IMPORTANTES:
  
  Inicia :	
  visualiza_vecindario/1
 
%==============================================================================================================%
*/
﻿:- use_module(library(clpfd)).

%	VERSION 1

%casa(<color>,<pais>)

visualiza_vecindario(V):- vecindario2(V).

vecindario2(Variables) :-
    Variables = [España, Noruega, Italia,
    		Rojo, Azul, Color3],
    Variables ins 1..3,
    País = [España, Noruega, Italia], all_distinct(País),
    Color = [Rojo, Azul, Color3], all_distinct(Color),
    (España #= Rojo-1) #\/ (España #= Rojo+1),			%1
    Noruega #= Azul,						%2
    Italia #= 2,						%3
    label(Variables).

%	OUTPUT
/*

Variables = [España, Noruega, Italia,
             Rojo, Azul, Color3],

?- vecindario2(V).
V = [1, 3, 2,
     2, 3, 1] ;
     
V = [3, 1, 2,
     2, 1, 3].


*/
