
%				EFRAIN CHAVEZ HERNANDEZ
%	   		 FUNDAMENTOS DE INTELIGENCIA ARTIFICIAL
%        		      DR.SALVADOR GODOY CALDERÓN
%				TAREA: laberinto.prolog

%		Programe predicados Prolog  para solucionar el laberinto que 
%	se inidica en la presentación #24  y que está  codificado  en  el  archivo:
%			  laberinto-01.prolog  (Corregido)
%	 
%	   Solucione buscando por los métodos  UniformCost, Greedy  y  A*...
%	      Añada también el predicado  intercambia_inicio_meta...
  

  
:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),max_depth(0),spacing(next_argument)]).  

:- use_module(library(clpfd)).
:- use_module(library(clpr)).

  
:- consult('laberinto-01.prolog').

% search(<[Start]>,<[Goal]>,Path).
search(StartNodes,Targets,Path):- 
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
	add_to_agenda(NewItems,More,NewAgenda),
	%append(NewItems,More,NewAgenda),
	do_search(Targets,NewAgenda,AFinalPath).

do_search(Targets,[_|More],AFinalPath):-
	do_search(Targets,More,AFinalPath).
	

% Definición de la función de aptitud para el elemento dado
% DISTANCIA HAMILTONIANA

buscaAptitud(Elemento, Target, Aptitud) :-
    Elemento = [X1, Y1],
    Target = [X2, Y2],
    Aptitud #= 1,
    Costo is X + Y + Niveles.

un_acceso(Nodo, NodoAcceso) :-
    acceso(Nodo, Accesos),
    member(NodoAcceso, Accesos).

sqrt_clpr(Number, Result) :-
    Result = sqrt(Number).











