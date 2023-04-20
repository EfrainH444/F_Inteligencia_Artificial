
/*
%==============================================================================================================%

         		              EFRAIN CHAVEZ HERNANDEZ
       			       FUNDAMENTOS DE INTELIGENCIA ARTIFICIAL
        		            DR.SALVADOR GODOY CALDERÓN
         		                TAREA: cebra.prolog
          Resuelva en Prolog  los tres problemas básicos de unificación que se encuentran al 
	       	          final de la presentación #16 (versiones recortadas:)
	       	          
		  			    VERSION 3
  
  PREDICADOS IMPORTANTES:
  
  Inicia :	
  visualiza_vecindario/1
 
 
%==============================================================================================================%
*/
:- use_module(library(clpfd)).

%	VERSION 3

%casa(<color>,<pais>,<deporte>,<mascota>)

visualiza_vecindario(V):- vecindario4(V).

vecindario4(Variables) :-
    Variables = [ Rusia, Escocia, Irlanda, País4, 
                  Negro, Rojo, Blanco, Color4,
                  Caballo, Tortuga, Mariposa, Mascota4,
                  Boliche, Tennis, Voleyball, Natacion],

    Variables ins 1..4, % Cada una de las 4 casas...

    País = [Rusia, Escocia, Irlanda, País4], all_distinct(País),
    Color = [Negro, Rojo, Blanco, Color4], all_distinct(Color),
    Mascota = [Caballo, Tortuga, Mariposa, Mascota4], all_distinct(Mascota),
    Deporte = [Boliche, Tennis, Voleyball, Natacion], all_distinct(Deporte),
    
    (Boliche #= Natacion - 3) #\/ (Boliche #= Natacion + 3),	%1
    (Irlanda #= Voleyball - 2) #\/ (Irlanda #= Voleyball + 2),	%2
    Negro #\= 2,						%3
    (Caballo #= Rojo - 2) #\/ (Caballo #= Rojo + 2),		%4
    (Escocia #= Tortuga - 1) #\/ (Escocia #= Tortuga + 1),	%5
    (Caballo #= Mariposa - 3) #\/ (Caballo #= Mariposa + 3),	%6
    Boliche #> Tennis,						%7	
    (Voleyball #= Blanco - 2) #\/ (Voleyball #= Blanco + 2),	%8
    Rusia #= 1,							%9
  
    label(Variables).


%	OUTPUT
/*

V = [Rusia,Escocia,Irlanda,País4, 
    Negro, Rojo, Blanco, Color4,
    Caballo, Tortuga, Mariposa, Mascota4,
    Boliche, Tennis, Voleyball, Natacion],

?- vecindario4(V).
V = [1, 2, 4, 3, 
     1, 2, 4, 3,
     4, 3, 1, 2, 
     4, 3, 2, 1] ;
     
V = [1, 2, 4, 3, 
     1, 3, 4, 2, 
     1, 3, 4, 2, 
     4, 3, 2, 1] ;
     
V = [1, 2, 4, 3, 
     3, 2, 4, 1, 
     4, 3, 1, 2, 
     4, 3, 2, 1] ;
     
V = [1, 3, 4, 2, 
     1, 2, 4, 3, 
     4, 2, 1, 3, 
     4, 3, 2, 1] ;
     
V = [1, 3, 4, 2, 
     1, 3, 4, 2, 
     1, 2, 4, 3, 
     4, 3, 2, 1] ;
     
V = [1, 3, 4, 2, 
     3, 2, 4, 1, 
     4, 2, 1, 3, 
     4, 3, 2, 1].

*/


