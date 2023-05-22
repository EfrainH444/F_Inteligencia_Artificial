
%				EFRAIN CHAVEZ HERNANDEZ
%	   		 FUNDAMENTOS DE INTELIGENCIA ARTIFICIAL
%        		   

%		Programe predicados Prolog  para solucionar el laberinto que 
%	se inidica en la presentación #24  y que está  codificado  en  el  archivo:
%			          laberinto-01.prolog  
%	   Solucione buscando por los métodos  UniformCost, Greedy  y  A*...
%	      Añada también el predicado  intercambia_inicio_meta...
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),max_depth(0),spacing(next_argument)]).  

:- use_module(library(clpfd)).
:- use_module(library(clpr)).

:- dynamic(edo_meta/1).

:- consult('laberinto-01.prolog').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% search(<[[]]>,<[[]]>,Path).
search(StartNodes,Targets,Path):-	
	retractall(edo_meta(_)),
	assert(edo_meta(Targets)),
	member(Node,StartNodes),
	do_search(Targets,[path(Node,[])],AFinalPath),
	reverse(AFinalPath,Path).
	
do_search(Targets,[path(Node,HowIGotThere)|More],AFinalPath):-
	member(Node,Targets),
	!,
	(AFinalPath = [Node|HowIGotThere]
	; do_search(Targets,More,AFinalPath)
	).

do_search(Targets,[path(Node,HowIGotThere)|More],AFinalPath):-
	setof(path(Succ,[Node|HowIGotThere]),
	( un_acceso(Node,Succ),
	  \+ (member(Succ,[Node|HowIGotThere]))
	
	),
	NewItems),!,
	sort_agenda(NewItems,More,NewAgenda),
	%append(NewItems,More,NewAgenda),
	do_search(Targets,NewAgenda,AFinalPath).

do_search(Targets,[_|More],AFinalPath):-
	do_search(Targets,More,AFinalPath).
	

% Definición de la funciones de distanica para el elemento dado
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%aptitud(<[]>,<[]>,A)
aptitud(Elemento, Aptitud) :-
    edo_meta(Target),
    Elemento = [X1, Y1],
    Target = [X2, Y2],
    Aptitud #= (X2-X1) + (Y2-Y1),!.
    
%costo(<[]>,<[[]]>,C)
costo(Elemento, Costo):-
    edo_meta(Target),
    do_search(Target,[path(Elemento,[])],Ruta),
    length(Ruta,Costo),!.
    
%chebyshev(<[]>,<[]>,C)
chebyshev(Elemento, Distancia) :-
    edo_meta(Target),
    Elemento = [X1, Y1],
    Target = [X2, Y2],
    DiferenciaX = abs(X1-X2),
    DiferenciaY = abs(Y1-Y2), 
    Distancia is max(DiferenciaX,DiferenciaY).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
%Predicdos para ordenar los nuevos estados:

%Uniform Cost
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inserta_costo(Nuevo,[],[Nuevo]).

inserta_costo(Nuevo,[X|Resto],[Nuevo,X|Resto]):-
	costo(Nuevo,C1),
	costo(X,C2),
	C1=<C2.

inserta_costo(Nuevo,[X|Resto1],[X|Resto2]):-
	inserta_costo(Nuevo,Resto1,Resto2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Greedy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inserta_greedy(Nuevo,[],[Nuevo]).

inserta_greedy(Nuevo,[X|Resto],[Nuevo,X|Resto]):-
	aptitud(Nuevo,A1),
	aptitud(X,A2),
	A1=<A2.

inserta_greedy(Nuevo,[X|Resto1],[X|Resto2]):-
	inserta_greedy(Nuevo,Resto1,Resto2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%A*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inserta_astar(Nuevo,[],[Nuevo]).

inserta_astar(Nuevo,[X|Resto],[Nuevo,X|Resto]):-
	aptitud(Nuevo,A1),costo(Nuevo,C1), AX1 is A1 + C1,
	aptitud(X,A2),costo(X,C2), AX2 is A2 + C2,
	AX1=<AX2.

inserta_astar(Nuevo,[X|Resto1],[X|Resto2]):-
	inserta_astar(Nuevo,Resto1,Resto2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Chebyshev
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inserta_chebyshev(Nuevo,[],[Nuevo]).

inserta_chebyshev(Nuevo,[X|Resto],[Nuevo,X|Resto]):-
	chebyshev(Nuevo,C1),
	chebyshev(X,C2),
	C1=<C2.

inserta_chebyshev(Nuevo,[X|Resto1],[X|Resto2]):-
	inserta_chebyshev(Nuevo,Resto1,Resto2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Predicado para ordenar la agenda
sort_agenda([],Agenda,Agenda).

sort_agenda([Edo|Resto],Agenda,Ordenada):- 
	inserta_chebyshev(Edo,Agenda, NuevaAgenda),
	sort_agenda(Resto,NuevaAgenda,Ordenada).
	
%Predicado para desplegar uno a uno los nodos a los que tiene acceso un nodo
un_acceso(Nodo, NodoAcceso) :-
    acceso(Nodo, Accesos),
    member(NodoAcceso, Accesos).






