/*
%==============================================================================================================%

         		          EFRAIN CHAVEZ HERNANDEZ
       			    FUNDAMENTOS DE INTELIGENCIA ARTIFICIAL
        		        DR.SALVADOR GODOY CALDERÓN
         		          TAREA: poker.prolog
    Construya un programa Prolog para reparatir cartas a 4 jugadores e identificar la mejor 
              figura que cada uno puede lograr con la mano que recibió.
  
  
  PREDICADOS IMPORTANTES:
  
  Iniciar el juego:	Reiniciar el juego:
  reparte_cartas().	reiniciar().
 
%==============================================================================================================%
*/

%======= PREDICADOS AUXILIARES ================================================================================%

%Configurar consola para mostrar todos los elementos de una lista
:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),max_depth(0),spacing(next_argument)]).

%Valida cartas consecutivos
consecutivos(X,Y) :-
    nth0(Xi, [2,3,4,5,6,7,8,9,10,'J','Q','K','A'], X),
    nth0(Yi, [2,3,4,5,6,7,8,9,10,'J','Q','K','A'], Y),
    succ(Xi, Yi).
    
%Valida si una carta es mayor que otra
mayor_que(X,Y) :-
    nth0(Xi, [2,3,4,5,6,7,8,9,10,'J','Q','K','A'], X),
    nth0(Yi, [2,3,4,5,6,7,8,9,10,'J','Q','K','A'], Y),
    Xi>Yi.
    
%Valida si una figura es mayor que otra
figura_mayor_que(X,Y) :-
    nth0(Xi, [nada,par,doble_par,tercia,escalera,color,full,poker,flor,flor_imperial], X),
    nth0(Yi, [nada,par,doble_par,tercia,escalera,color,full,poker,flor,flor_imperial], Y),
    Xi>Yi.

%Devuelve la carta mayor entre dos cartas
mayor(X,Y,M) :-
    nth0(Xi, [2,3,4,5,6,7,8,9,10,'J','Q','K','A'], X),
    nth0(Yi, [2,3,4,5,6,7,8,9,10,'J','Q','K','A'], Y),
    Xi>Yi,M = X.


mayor(X,Y,M) :-
    nth0(Xi, [2,3,4,5,6,7,8,9,10,'J','Q','K','A'], X),
    nth0(Yi, [2,3,4,5,6,7,8,9,10,'J','Q','K','A'], Y),
    Yi>Xi,M = Y.

%Remueve un elemento de la lista, solamente una vez.
quita_elemento(X, [X|Xs], Xs). 
quita_elemento(X, [Y|Ys], [Y|Zs]):- quita_elemento(X, Ys, Zs),!.

%Remover cartas de una lista mano de la lista mazo
%Caso base: si la lista esta vacía ya terminamos de remover cartas, devolvemos Mazo = Mazo sin Cartas
remover_cartas(Mazo, [], Mazo).
%Caso recursivo: si la carta es la primera, la remueve con select y se llama recursivamente con Mano Restant y Mazo sin cartas
remover_cartas(Mazo, [Carta|ManoRestante], MazoSinCartas) :-
    select(Carta, Mazo, NuevoMazo),
    remover_cartas(NuevoMazo, ManoRestante, MazoSinCartas).
%Mazo sin cartas es la lista final sin cartas en el mazo que se incluian en la mano
   
%======= MODELADO DEL CONOCIMIENTO  ==========================================================================================%

%Palo con sus representaciones en UNICODE
palo(P):-member(P,['\u2660','\u2665','\u2663','\u2666']).
%Personajes
personaje(P):- member(P,['A','J','Q','K']).
%Valores
valor(V):- between(2,10,V).
%Comodines
comodín(C):- member(C,['JK1','JK2','JK3','JK4']).

%Crea cartas de personajes y valores
carta_personaje(Personaje-Palo):-personaje(Personaje),palo(Palo).
carta_valor(Valor-Palo):-valor(Valor),palo(Palo).

%Genera una baraja
baraja(B):- findall(C,carta_personaje(C),Personajes),findall(C,carta_valor(C),Valores),
	    findall(C,comodín(C),Comodines),append(Personajes,Valores,Cartas),append(Comodines,Cartas,B).

%Ordena la baraja aleatoriamente
barajar(Baraja,Barajada):-baraja(Baraja), random_permutation(Baraja,Barajada).

%Genera un mazo; contiene 2 barajas en orden aleatorio 
mazo(Mazo):-findall(B,barajar(_,B),Mitad),findall(B,barajar(_,B),Mitad2),append(Mitad,Mitad2,Maz),flatten(Maz,Mazo).

%Obtiene una carta al azar del mazo
carta_al_azar(Mazo,Carta):- length(Mazo,N),N>=2,random_between(1,N,Pos),nth1(Pos,Mazo,Carta).

%Obtiene una lista de tamaño NumCartas al azar
mano(Mazo,NumCartas,Mano):- length(Mano,NumCartas),maplist(carta_al_azar(Mazo),Mano).

%Obtiene 4 manos de 5 cartas 
manos(Mano,Mano2,Mano3,Mano4):- mazo(Mazo), mano(Mazo,5,Mano), remover_cartas(Mazo,Mano,M2),
					mano(M2,5,Mano2),remover_cartas(M2,Mano2,M3),
					mano(M3,5,Mano3),remover_cartas(M3,Mano3,M4),
					mano(M4,5,Mano4).

%Recibe una mano y devuelve la figura que forma
figura(Mano,Figura):- mano_flor_imperial(Mano,V),!,append([],[flor_imperial],Figura).
figura(Mano,Figura):- mano_flor(Mano,V),!,append([],[flor],Figura).
figura(Mano,Figura):- mano_poker(Mano,V),!,append([],[poker],Figura).
figura(Mano,Figura):- mano_full(Mano,V),!,append([],[full],Figura).
figura(Mano,Figura):- mano_color(Mano,V),!,append([],[color],Figura).
figura(Mano,Figura):- mano_escalera(Mano,V),!,append([],[escalera],Figura).
figura(Mano,Figura):- mano_tercia(Mano,V),!,append([],[tercia],Figura).
figura(Mano,Figura):- mano_doble_par(Mano,V),!,append([],[doble_par],Figura).
figura(Mano,Figura):- mano_par(Mano,V),append([],[par],Figura).
figura(Mano,Figura):- mano_nada(Mano),!,append([],[nada],Figura).

%Predicado para crear una mano y probar 
figura_prueba(Resultado):- append([],['Q'-'/u2663','JK1','A'-'/u2663','J'-'/u2660',10-'/u2660'],Resultado).
%prueba(Mano):- figura(Mano).

reparte_cartas():- format("Generando mano de cada jugador... ~n"),
		   obten_figuras(M,M2,M3,M4,F1,F2,F3,F4),
		   format("Resultado: ~n"),
		   format("Lugar	    Jugador	  Figura		Mano ~n"),
		   format("================================================================================= ~n"),
		   format(" X         Jugador  X     ~w		",[F1]),format("~w ~n",[M]),
		   format(" X         Jugador  X     ~w		",[F2]),format("~w ~n",[M2]),
		   format(" X         Jugador  X     ~w		",[F3]),format("~w ~n",[M3]),
		   format(" X         Jugador  X     ~w		",[F4]),format("~w ~n",[M4]),
		   format("================================================================================= ~n").
		   
		   
obten_figuras(Mano,Mano2,Mano3,Mano4,F1,F2,F3,F4):- manos(Mano,Mano2,Mano3,Mano4),figura(Mano,F1),figura(Mano2,F2),
						    figura(Mano3,F3),figura(Mano4,F4),!.
						    
%Cambiar metodo obten figuras por ordena figuras que devuelve las manos y las figuras en orden

reinicia():- reparte_cartas().
%======= FIGURAS  ==========================================================================================================%

mano_nada(Mano):- member(V-P,Mano),select(V-P,Mano,Resto1),
	          member(V2-P2,Resto1),select(V2-P2,Resto1,Resto2),
	          member(V3-P3,Resto2),select(V3-P3,Resto2,Resto3),
       	          member(V4-P4,Resto3),select(V4-P4,Resto3,Resto4),
                  member(V5-_,Resto4),(V\==V2),(V\==V3),(V\==V4),(V\==V5),
                 (V2\== V3),(V2\== V4),(V2 \== V5),(V3\==V4),(V3\==V5),(V4\==V5),
	         (V3 \== V),(V4 \== V),(V5 \== V),(P\==P2),!.

mano_par(Mano,V):- member(V-_,Mano),select(V-_,Mano,Resto1),
		 member(V2-_,Resto1),member(V3-_,Resto1),
		 member(V4-_,Resto1),member(V5-_,Resto1),
		 V2 == V,(V3\== V4),(V3 \== V5),(V5 \== V4),
	         (V3 \== V),(V4 \== V),(V5 \== V),!.
	       
mano_par(Mano,V1):-member(V1-_,Mano),select(V1-_,Mano,Resto1),
	           member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
	           member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		   member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
		   member(C,Resto4),comodín(C),
 	           (V1 \== V2),(V1 \== V3),(V1 \== V4),
		   (V2 \== V3),(V2 \== V4),(V3 \== V4),
		   mayor_que(V1,V2),mayor_que(V2,V3),
	           mayor_que(V3,V4),!.

mano_tercia(Mano,V):- member(V-_,Mano),select(V-_,Mano,Resto1),
		    member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
		    member(V3-_,Resto2),member(V4-_,Resto2),
		    member(V5-_,Resto2),V2 == V,V3 == V,
		    (V5 \== V4),(V4 \== V),(V5 \== V),!.

mano_tercia(Mano,V):- member(V-_,Mano),select(V-_,Mano,Resto1),
		    member(V2-_,Resto1),member(V3-_,Resto1),
		    member(V4-_,Resto1),member(C,Resto1),comodín(C),
		    V2 == V,V3 == V,(V4 \== V),!.

mano_tercia(Mano,V):- member(V-_,Mano),select(V-_,Mano,Resto1),
	          member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
	          member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
       	          member(C,Resto3),comodín(C),quita_elemento(C,Resto3,Resto4),
                  member(C2,Resto4),comodín(C2),
		  V \== V2, V \== V3, V2 \== V3,
		  mayor_que(V,V2),mayor_que(V2,V3),!.
		
mano_doble_par(Mano,V):- member(V-_,Mano),select(V-_,Mano,Resto1),
		       member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
		       member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		       member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
	               member(V5-_,Resto4),mayor_que(V,V3), V == V2,V3 == V4,(V \== V3),
	               (V \== V5),(V3 \== V5),!.
	               
mano_escalera(Mano,V5):- member(V-_,Mano),select(V-_,Mano,Resto1),
		      member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
		      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
	              member(V5-_,Resto4),consecutivos(V,V2),consecutivos(V2,V3),
	              consecutivos(V3,V4),consecutivos(V4,V5),!.

mano_escalera(Mano,V5):- member(C,Mano),comodín(C),select(C,Mano,Resto1),
		      member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
		      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
	              member(V5-_,Resto4),consecutivos(V2,V3),consecutivos(V3,V4),
	              consecutivos(V4,V5),!.	
	             
mano_escalera(Mano,V5):- member(C,Mano),comodín(C),select(C,Mano,Resto1),
		      member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
		      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
	              member(V5-_,Resto4), consecutivos(V2,VA),consecutivos(VA,V3),
	              consecutivos(V3,V4),consecutivos(V4,V5),!.
	             	 
mano_escalera(Mano,V5):- member(C,Mano),comodín(C),select(C,Mano,Resto1),
		      member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
		      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
	              member(V5-_,Resto4), consecutivos(V2,V3),consecutivos(V3,V4),
	              consecutivos(V4,VA),consecutivos(VA,V5),!.

mano_escalera(Mano,V5):- member(C,Mano),comodín(C),select(C,Mano,Resto1),
		      member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
		      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
	              member(V5-_,Resto4), consecutivos(V2,V3),consecutivos(V3,VA),
	              consecutivos(VA,V4),consecutivos(V4,V5),!.	            
	              	                          
mano_escalera(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		      member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
		      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
	              member(V5-_,Resto4),consecutivos(V3,V4),consecutivos(V4,V5),!.
	              
mano_escalera(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		      member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
		      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
	              member(V5-_,Resto4),consecutivos(V3,VA),consecutivos(VA,VB),
                      consecutivos(VB,V4),consecutivos(V4,V5),!.

mano_escalera(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		      member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
		      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
	              member(V5-_,Resto4),consecutivos(V3,V4),consecutivos(V4,VA),
                      consecutivos(VA,VB),consecutivos(VB,V5),!.
                      
mano_escalera(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		      member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
		      member(C3,Resto2),comodín(C3),quita_elemento(C3,Resto2,Resto3),
		      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
		      member(V5-_,Resto4),consecutivos(V4,V5),!.   
		      
mano_escalera(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		      member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
		      member(C3,Resto2),comodín(C3),quita_elemento(C3,Resto2,Resto3),
		      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
		      member(V5-_,Resto4),consecutivos(V4,VA),consecutivos(VA,VB),
                      consecutivos(VB,VC),consecutivos(VC,V5),!.		                    
	              
	              
mano_color(Mano,V):- member(V-P,Mano),select(V-P,Mano,Resto1),
		   member(V2-P2,Resto1),select(V2-P2,Resto1,Resto2),
	           member(V3-P3,Resto2),select(V3-P3,Resto2,Resto3),
	           member(V4-P4,Resto3),select(V4-P4,Resto3,Resto4),
	           member(V5-P5,Resto4),(P==P2),(P==P3),
	           (P==P4),(P==P5),(P2==P3),(P2==P4),(P2==P5),
	           (P3==P4),(P3==P5),(P4==P5),mayor_que(V,V2),mayor_que(V2,V3),
	           mayor_que(V3,V4),mayor_que(V4,V5),!.
	           
mano_color(Mano,V2):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		   member(V2-P2,Resto1),select(V2-P2,Resto1,Resto2),
	           member(V3-P3,Resto2),select(V3-P3,Resto2,Resto3),
	           member(V4-P4,Resto3),select(V4-P4,Resto3,Resto4),
	           member(V5-P5,Resto4),(P2==P3),(P2==P4),(P2==P5),
	           (P3==P4),(P3==P5),(P4==P5),mayor_que(V2,V3),
	           mayor_que(V3,V4),mayor_que(V4,V5),!.

mano_color(Mano,V3):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		   member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
	           member(V3-P3,Resto2),select(V3-P3,Resto2,Resto3),
	           member(V4-P4,Resto3),select(V4-P4,Resto3,Resto4),
	           member(V5-P5,Resto4),(P3==P4),(P3==P5),(P4==P5),
	           mayor_que(V3,V4),mayor_que(V4,V5),!.
	           
mano_color(Mano,V4):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		   member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
	           member(C3,Resto2),comodín(C3),quita_elemento(C2,Resto2,Resto3),
	           member(V4-P4,Resto3),select(V4-P4,Resto3,Resto4),
	           member(V5-P5,Resto4),(P4==P5),mayor_que(V4,V5),!.           	           	          
	           
mano_full(Mano,V1):- member(V-_,Mano),select(V-_,Mano,Resto1),
	          member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
	          member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
                  member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
                  member(V5-_,Resto4),V == V2, V3 == V4, V3 == V5,
                  V4 == V5, V \== V3, V \== V4, V \== V5, V2 \== V3,
                  V2 \== V4, V \== V5,mayor(V,V3,V1),!. 
                  
mano_full(Mano,V1):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		  member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
	          member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		  member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
	          member(V5-_,Resto4), V2 == V3,V4 == V5,(V2 \== V4),
	          (V2 \== V5),(V3 \== V5),(V3 \== V4),mayor(V2,V5,V1),!.                  

mano_poker(Mano,V):- member(V-_,Mano),select(V-_,Mano,Resto1),
	           member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
	           member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
                   member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
                   member(V5-_,Resto4), V == V2, V == V3, V == V4,
                   V \== V5, V2 \== V5, V3 \== V5, V4 \== V5,!. 
                   
mano_poker(Mano,V):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		  member(V-_,Resto1),select(V-_,Resto1,Resto2),
	          member(V2-_,Resto2),select(V2-_,Resto2,Resto3),
	          member(V3-_,Resto3),member(V4-_,Resto3),
	          V == V2,V == V3,(V4 \== V),!.                    

mano_poker(Mano,V3):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		   member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
		   member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
		   member(V4-_,Resto3),V3 == V4, !.
		   
mano_poker(Mano,V1):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
		   member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
		   member(C3,Resto2),comodín(C3),quita_elemento(C3,Resto2,Resto3),
		   member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
		   member(V5-_,Resto4), V4 \== V5,mayor(V4,V5,V1),!. 


mano_flor(Mano,V):- mano_escalera(Mano,V),mano_color(Mano,V),!.       

mano_flor(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
			   member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
			   member(C3,Resto2),comodín(C3),quita_elemento(C3,Resto2,Resto3),
			   member(C4,Resto3),comodín(C4), member(V5-_,Resto3),
			   between(2,9,V5),!.        			           
                  
mano_flor_imperial(Mano,V5):- member(V-P,Mano),select(V-P,Mano,Resto1),
		  	   member(V2-P2,Resto1),select(V2-P2,Resto1,Resto2),
		   	   member(V3-P3,Resto2),select(V3-P3,Resto2,Resto3),
       	         	   member(V4-P4,Resto3),select(V4-P4,Resto3,Resto4),
	      	           member(V5-P5,Resto4),consecutivos(V,V2),consecutivos(V2,V3),
		 	   consecutivos(V3,V4),consecutivos(V4,V5),(P==P2),(P==P3),
	       		   (P==P4),(P==P5),(P2==P3),(P2==P4),(P2==P5),
	       		   (P3==P4),(P3==P5),(P4==P5),subset([V,V2,V3,V4,V5],[10,'J','Q','K','A']),!.
               		   
mano_flor_imperial(Mano,V5):- member(C,Mano),comodín(C),select(C,Mano,Resto1),
			      member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
			      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
			      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
			      member(V5-_,Resto4),consecutivos(V2,V3),consecutivos(V3,V4),
			      consecutivos(V4,V5),subset([V2,V3,V4,V5],[10,'J','Q','K','A']),!.	
	             
mano_flor_imperial(Mano,V5):- member(C,Mano),comodín(C),select(C,Mano,Resto1),
			      member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
			      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
			      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
			      member(V5-_,Resto4), consecutivos(V2,VA),consecutivos(VA,V3),
			      consecutivos(V3,V4),consecutivos(V4,V5),subset([V2,V3,V4,V5],[10,'J','Q','K','A']),!.
	             	 
mano_flor_imperial(Mano,V5):- member(C,Mano),comodín(C),select(C,Mano,Resto1),
			      member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
			      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
			      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
			      member(V5-_,Resto4), consecutivos(V2,V3),consecutivos(V3,V4),
			      consecutivos(V4,VA),consecutivos(VA,V5),subset([V2,V3,V4,V5],[10,'J','Q','K','A']),!.

mano_flor_imperial(Mano,V5):- member(C,Mano),comodín(C),select(C,Mano,Resto1),
			      member(V2-_,Resto1),select(V2-_,Resto1,Resto2),
			      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
			      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
			      member(V5-_,Resto4), consecutivos(V2,V3),consecutivos(V3,VA),
			      consecutivos(VA,V4),consecutivos(V4,V5),subset([V2,V3,V4,V5],[10,'J','Q','K','A']),!.	            
	              	                          
mano_flor_imperial(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
			      member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
			      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
			      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
			      member(V5-_,Resto4),consecutivos(V3,V4),consecutivos(V4,V5),
			      subset([V3,V4,V5],[10,'J','Q','K','A']),!.
	              
mano_flor_imperial(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
			      member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
			      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
			      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
			      member(V5-_,Resto4),consecutivos(V3,VA),consecutivos(VA,VB),
		              consecutivos(VB,V4),consecutivos(V4,V5),subset([V3,V4,V5],[10,'J','Q','K','A']),!.

mano_flor_imperial(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
			      member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
			      member(V3-_,Resto2),select(V3-_,Resto2,Resto3),
			      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
			      member(V5-_,Resto4),consecutivos(V3,V4),consecutivos(V4,VA),
		              consecutivos(VA,VB),consecutivos(VB,V5),subset([V3,V4,V5],[10,'J','Q','K','A']),!.
                      
mano_flor_imperial(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
			      member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
			      member(C3,Resto2),comodín(C3),quita_elemento(C3,Resto2,Resto3),
			      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
			      member(V5-_,Resto4),consecutivos(V4,V5),subset([V4,V5],[10,'J','Q','K','A']),!.   
		      
mano_flor_imperial(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
			      member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
			      member(C3,Resto2),comodín(C3),quita_elemento(C3,Resto2,Resto3),
			      member(V4-_,Resto3),select(V4-_,Resto3,Resto4),
			      member(V5-_,Resto4),consecutivos(V4,VA),consecutivos(VA,VB),
		              consecutivos(VB,VC),consecutivos(VC,V5),
		              subset([V4,V5],[10,'J','Q','K','A']),!.               		   
		       		   
               		   
mano_flor_imperial(Mano,V5):- member(C,Mano),comodín(C),quita_elemento(C,Mano,Resto1),
			   member(C2,Resto1),comodín(C2),quita_elemento(C2,Resto1,Resto2),
			   member(C3,Resto2),comodín(C3),quita_elemento(C3,Resto2,Resto3),
			   member(C4,Resto3),comodín(C4), member(V5-_,Resto3),
			   member(V5,[10,'J','Q','K','A']),!.		

	   





			   
			    
			    


