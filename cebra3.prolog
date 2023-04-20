

/*
%==============================================================================================================%

         		              EFRAIN CHAVEZ HERNANDEZ
       			       FUNDAMENTOS DE INTELIGENCIA ARTIFICIAL
        		            DR.SALVADOR GODOY CALDERÓN
         		                TAREA: cebra.prolog
          Resuelva en Prolog  los tres problemas básicos de unificación que se encuentran al 
	       	          final de la presentación #16 (versiones recortadas:)
	       	          
		  			    VERSION 2
  
  
  PREDICADOS IMPORTANTES:
  
  Inicia :	
  visualiza_vecindario/1
 
%==============================================================================================================%
*/
:- use_module(library(clpfd)).

%	VERSION 2

%casa(<color>,<pais>,<deporte>,<mascota>)

visualiza_vecindario(V):- vecindario3(V).

vecindario3(Variables) :-
    Variables = [Brasil, Alemania, País3, 
                 Verde, Rojo, Color3,
                 Perro, Pez, Gato,
                 Baloncesto, Fútbol, Deporte3],

    Variables ins 1..3, % Cada una de las 3 casas...

    País = [Brasil, Alemania, País3 ], all_distinct(País),
    Color = [Rojo, Verde, Color3], all_distinct(Color),
    Mascota = [Perro, Pez, Gato], all_distinct(Mascota),
    Deporte = [Baloncesto, Fútbol, Deporte3], all_distinct(Deporte),
    
    Brasil #\= 2,					%1
    Perro #= Baloncesto,				%2
    (Fútbol #= Rojo - 2) #\/ (Fútbol #= Rojo + 2),	%3
    (Pez #= Gato - 1) #\/ (Pez #= Gato + 1),		%4
    (Perro #= Verde - 1) #\/ (Perro #= Verde + 1),	%5
    Alemania #= 3,					%6
  
    label(Variables).
    
%	OUTPUT
/*
V = [Brasil, Alemania, País3, 
     Verde, Rojo, Color3,
     Perro, Pez, Gato,
     Baloncesto, Fútbol, Deporte3],

?- vecindario3(V).
V = [1, 3, 2, 
     2, 1, 3, 
     1, 2, 3, 
     1, 3, 2] ;
     
V = [1, 3, 2, 
     2, 1, 3, 
     1, 3, 2, 
     1, 3, 2] ;

V = [1, 3, 2, 
     2, 3, 1, 
     3, 1, 2, 
     3, 1, 2] ;

V = [1, 3, 2, 
     2, 3, 1, 
     3, 2, 1, 
     3, 1, 2] ;

*/

