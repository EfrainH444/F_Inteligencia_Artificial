/*================================================================================
				EFRAIN CHAVEZ HERNANDEZ
       			    FUNDAMENTOS DE INTELIGENCIA ARTIFICIAL
        		       DR.SALVADOR GODOY CALDERÓN
         		    TAREA: misioneros_canibales.prolog
  
  
  PREDICADOS IMPORTANTES:
  
  busca_DFS(<Ei>,<Em>)
  busca_BFS(<Ei>,<Em>)
  despliega(<plan>)
  
================================================================================*/
/*
edo_meta().

:- dynamic(edo_meta/1).
*/

:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),max_depth(0),spacing(next_argument)]).

%BÚSQUEDA 

search(StartNodes,Targets,Path):- 
	member(Node,StartNodes),
	do_search(Targets,[path(Node,[])],AFinalPath),
	reverse(AFinalPath,Path),
	despliega(Path).
	
do_search(Targets,[path(Node,HowIGotThere)|More],AFinalPath):-
	member(Node,Targets),
	!,
	(AFinalPath = [Node|HowIGotThere]
	; do_search(Targets,More,AFinalPath)
	).

do_search(Targets,[path(Node,HowIGotThere)|More],AFinalPath):-
	setof(path(Succ,[Node|HowIGotThere]),
	( sucesor([Node],Succ),
	  \+ (member(Succ,[Node|HowIGotThere]))
	
	),
	NewItems),!,
	%add_to_agenda(NewItems,More,NewAgenda),
	append(NewItems,More,NewAgenda),
	do_search(Targets,NewAgenda,AFinalPath).

do_search(Targets,[_|More],AFinalPath):-
	do_search(Targets,More,AFinalPath).


%CALCULA LOS SUCESORES
sucesores([Edo|Resto], Sucesores):-
	findall([S,Edo|Resto],
	(movimiento(Edo,S), \+ member(S,[Edo|Resto])),
	Sucesores).
	
sucesor([Edo|Resto], S):-
	movimiento(Edo,S), 
	\+ member(S,[Edo|Resto]).

	
movimiento([MO,CO,MD,CD,L1], [MO2,CO2,MD2,CD2,L2]):-
	% Un misionero
	((MO2 is MO-1,CO2 is CO,MD2 is MD+1,CD2 is CD,L1='o',L2='d');
	(MO2 is MO+1,CO2 is CO,MD2 is MD-1,CD2 is CD,L1='d',L2='o');

	%Dos misioneros
	(MO2 is MO-2,CO2 is CO,MD2 is MD+2,CD2 is CD,L1='o',L2='d');
	(MO2 is MO+2,CO2 is CO,MD2 is MD-2,CD2 is CD,L1='d',L2='o');
	%Misionero y caníbal
	(MO2 is MO-1,CO2 is CO-1,MD2 is MD+1,CD2 is CD+1,L1='o',L2='d');
	(MO2 is MO+1,CO2 is CO+1,MD2 is MD-1,CD2 is CD-1,L1='d',L2='o');
	%Un caníbal
	(MO2 is MO,CO2 is CO-1,MD2 is MD,CD2 is CD+1,L1='o',L2='d');
	(MO2 is MO,CO2 is CO+1,MD2 is MD,CD2 is CD-1,L1='d',L2='o');
	%Dos canibales
	(MO2 is MO,CO2 is CO-2,MD2 is MD,CD2 is CD+2,L1='o',L2='d');
	(MO2 is MO,CO2 is CO+2,MD2 is MD,CD2 is CD-2,L1='d',L2='o')
	),
	edo_válido([MO2,CO2,MD2,CD2]).


movimiento2([MO,CO,MD,CD,L1], [MO2,CO2,MD2,CD2,L2],MOV):-
	% Un misionero
	((MO2 is MO-1,CO2 is CO,MD2 is MD+1,CD2 is CD,L1='o',L2='d'),
	MOV = [un_misionero];
	(MO2 is MO+1,CO2 is CO,MD2 is MD-1,CD2 is CD,L1='d',L2='o'),
	MOV = [un_misionero];
	%Dos misioneros
	(MO2 is MO-2,CO2 is CO,MD2 is MD+2,CD2 is CD,L1='o',L2='d'),
	MOV = [dos_misioneros];
	(MO2 is MO+2,CO2 is CO,MD2 is MD-2,CD2 is CD,L1='d',L2='o'),
	MOV = [dos_misioneros];
	%Misionero y caníbal
	(MO2 is MO-1,CO2 is CO-1,MD2 is MD+1,CD2 is CD+1,L1='o',L2='d'),
	MOV = [misionero_y_caníbal];
	(MO2 is MO+1,CO2 is CO+1,MD2 is MD-1,CD2 is CD-1,L1='d',L2='o'),
	MOV = [misionero_y_caníbal];
	%Un caníbal
	(MO2 is MO,CO2 is CO-1,MD2 is MD,CD2 is CD+1,L1='o',L2='d'),
	MOV = [un_caníbal];
	(MO2 is MO,CO2 is CO+1,MD2 is MD,CD2 is CD-1,L1='d',L2='o'),
	MOV = [un_caníbal];
	%Dos canibales
	(MO2 is MO,CO2 is CO-2,MD2 is MD,CD2 is CD+2,L1='o',L2='d'),
	MOV =  [dos_canibales];
	(MO2 is MO,CO2 is CO+2,MD2 is MD,CD2 is CD-2,L1='d',L2='o'),
	MOV =  [dos_canibales]
	),
	edo_válido([MO2,CO2,MD2,CD2]).

edo_válido([Mis1,Can1,Mis2,Can2]):-
	Mis1 >= 0, Can1 >= 0,
	Mis2 >= 0, Can2 >= 0,
	(Mis1 >= Can1; Mis1 is 0),
	(Mis2 >= Can2; Mis2 is 0).

despliega([Estado|Resto]):- L = [Estado|Resto], length(L,Tamaño),
	format(" ~n ~nÉxito, solución con ~w pasos ~n",[Tamaño]),
	format("Inicio en: "), despliega_estado_2(Estado),
	aux_simplificado(L,0,_),!.


aux_simplificado(Ruta,Incremento,Incremento):- length(Ruta,Tamaño), Incremento is Tamaño-1,!.

aux_simplificado(Ruta,Incremento,IncrementoT):- Incremento2 is Incremento + 1,
   nth0(Incremento,Ruta,Actual),nth0(Incremento2,Ruta,Siguiente),
   movimiento2(Actual,Siguiente,Mov),
   despliega_estado(Siguiente,Incremento,Mov),
   aux_simplificado(Ruta,Incremento2,IncrementoT).


% Predicado para desplegar un estado
despliega_estado([MO,CO,MD,CD,_],It,Mov) :-
    format("~w) Aplicando	~w		se llega a	((~w ~w) (~w ~w)) ~n",[It,Mov,MO,CO,MD,CD]).

despliega_estado_2([MO,CO,MD,CD,_]) :-
    format("((~w ~w) (~w ~w)) ~n",[MO,CO,MD,CD]).

	
	
