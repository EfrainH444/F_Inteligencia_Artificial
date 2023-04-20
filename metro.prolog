/*================================================================================
				EFRAIN CHAVEZ HERNANDEZ
       			    FUNDAMENTOS DE INTELIGENCIA ARTIFICIAL
        		        DR.SALVADOR GODOY CALDERÓN
         		          TAREA: metro.prolog
    Elabore un programa Prolog  para  asistir a un viajero en el la red del Sistema Colectivo 
      Metro de la CdMx.El programa debe resolver 3 predicados fundamentales:  mejor_ruta/4, 
  reporte_tiempo/2 y reporte_simplificado/2 tal como lo indican las láminas de la presentación #19...
  
  
  PREDICADOS IMPORTANTES:
  
  mejor_ruta(Inicio,Destino,Ruta).
  reporte_tiempo(Inicio,Destino).
  reporte_simplificado(Inicio,Destino).
  
  Notas importantes:
  Los predicados tardan hasta 4 minutos en dar respuesta.
  Verificar que las libreria clpfd se importe correctamente.
  
================================================================================*/
﻿:-use_module(library(clpfd)).
:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),max_depth(0),spacing(next_argument)]).

%-----------------------------------------------------
% Parámetros globales...
%-----------------------------------------------------
valor_parámetro(tiempo_inicial, 8).
valor_parámetro(tiempo_tramo, 5).
valor_parámetro(tiempo_transbordo, 10).
valor_parámetro(tiempo_final, 7).

%-----------------------------------------------------
% Definición de las líneas...
%-----------------------------------------------------
color(línea_1, rosa).
color(línea_2, azul_marino).
color(línea_3, verde_olivo).
color(línea_4, azul_cielo).
color(línea_5, amarillo).
color(línea_6, rojo).
color(línea_7, naranja).
color(línea_8, verde_bandera).
color(línea_9, café).
color(línea_A, morado).
color(línea_B, gris_verde).
color(línea_12, dorado).

trayecto(línea_1, observatorio, pantitlán).
trayecto(línea_2, cuatro_caminos, tasqueña).
trayecto(línea_3, indios_verdes, universidad).
trayecto(línea_4, martín_carrera, santa_anita).
trayecto(línea_5, politécnico, pantitlán).
trayecto(línea_6, el_rosario, martín_carrera).
trayecto(línea_7, el_rosario, barranca_del_muerto).
trayecto(línea_8, garibaldi_lagunilla, constitución_de_1917).
trayecto(línea_9, tacubaya, pantitlán).
trayecto(línea_A, pantitlán, la_paz).
trayecto(línea_B, buenavista, ciudad_azteca).
trayecto(línea_12, mixcoac, tláhuac).

%-----------------------------------------------------
% Línea 1: Observatorio _ Pantitlán
%-----------------------------------------------------
sigue(observatorio, tacubaya, línea_1).
sigue(tacubaya, juanacatlán, línea_1).
sigue(juanacatlán, chapultepec, línea_1).
sigue(chapultepec, sevilla, línea_1).
sigue(sevilla, insurgentes, línea_1).
sigue(insurgentes, cuauhtémoc, línea_1).
sigue(cuauhtémoc, balderas, línea_1).
sigue(balderas, salto_del_agua, línea_1).
sigue(salto_del_agua, isabel_la_católica, línea_1).
sigue(isabel_la_católica, pino_suárez, línea_1).
sigue(pino_suárez, merced, línea_1).
sigue(merced, candelaria, línea_1).
sigue(candelaria, san_lázaro, línea_1).
sigue(san_lázaro, moctezuma, línea_1).
sigue(moctezuma, balbuena, línea_1).
sigue(balbuena, boulvd_puerto_aéreo, línea_1).
sigue(boulvd_puerto_aéreo, gómez_farías, línea_1).
sigue(gómez_farías, zaragoza, línea_1).
sigue(zaragoza, pantitlán, línea_1).

%-----------------------------------------------------
% Línea 2: Cuatro Caminos _  Tasqueña
%-----------------------------------------------------
sigue(cuatro_caminos, panteones, línea_2).
sigue(panteones, tacuba, línea_2).
sigue(tacuba, cuitláhuac, línea_2).
sigue(cuitláhuac, popotla, línea_2).
sigue(popotla, colegio_militar, línea_2).
sigue(colegio_militar, normal, línea_2).
sigue(normal, san_cosme, línea_2).
sigue(san_cosme, revolución, línea_2).
sigue(revolución, hidalgo, línea_2).
sigue(hidalgo, bellas_artes, línea_2).
sigue(bellas_artes, allende, línea_2).
sigue(allende, zócalo, línea_2).
sigue(zócalo, pino_suárez, línea_2).
sigue(pino_suárez, san_antonio_abad, línea_2).
sigue(san_antonio_abad, chabacano, línea_2).
sigue(chabacano, viaducto, línea_2).
sigue(viaducto, xola, línea_2).
sigue(xola, villa_de_cortés, línea_2).
sigue(villa_de_cortés, nativitas, línea_2).
sigue(nativitas, portales, línea_2).
sigue(portales, ermita, línea_2).
sigue(ermita, general_anaya, línea_2).
sigue(general_anaya, tasqueña, línea_2).

%-----------------------------------------------------
% Línea 3: Indios Verdes _ Universidad
%-----------------------------------------------------
sigue(indios_verdes, deportivo_18_de_marzo, línea_3).
sigue(deportivo_18_de_marzo, potrero, línea_3).
sigue(potrero, la_raza, línea_3).
sigue(la_raza, tlatelolco, línea_3).
sigue(tlatelolco, guerrero, línea_3).
sigue(guerrero, hidalgo, línea_3).
sigue(hidalgo, juárez, línea_3).
sigue(juárez, balderas, línea_3).
sigue(balderas, niños_héroes, línea_3).
sigue(niños_héroes, hospital_general, línea_3).
sigue(hospital_general, centro_médico, línea_3).
sigue(centro_médico, etiopía, línea_3).
sigue(etiopía, eugenia, línea_3).
sigue(eugenia, división_del_norte, línea_3).
sigue(división_del_norte, zapata, línea_3).
sigue(zapata, coyoacán, línea_3).
sigue(coyoacán, viveros, línea_3).
sigue(viveros, miguel_ángel_de_quevedo, línea_3).
sigue(miguel_ángel_de_quevedo, copilco, línea_3).
sigue(copilco, universidad, línea_3).

%-----------------------------------------------------
% Línea 4: Martín Carrera _ Santa Anita
%-----------------------------------------------------
sigue(martín_carrera, talismán, línea_4).
sigue(talismán, bondojito, línea_4).
sigue(bondojito, consulado, línea_4).
sigue(consulado, canal_del_norte, línea_4).
sigue(canal_del_norte, morelos, línea_4).
sigue(morelos, candelaria, línea_4).
sigue(candelaria, fray_servando, línea_4).
sigue(fray_servando, jamaica, línea_4).
sigue(jamaica, santa_anita, línea_4).

%-----------------------------------------------------
% Línea 5: Politécnico _ Pantitlán
%-----------------------------------------------------
sigue(politécnico, instituto_del_petróleo, línea_5).
sigue(instituto_del_petróleo, autobuses_del_norte, línea_5).
sigue(autobuses_del_norte, la_raza, línea_5).
sigue(la_raza, misterios, línea_5).
sigue(misterios, valle_gómez, línea_5).
sigue(valle_gómez, consulado, línea_5).
sigue(consulado, eduardo_molina, línea_5).
sigue(eduardo_molina, aragón, línea_5).
sigue(aragón, oceanía, línea_5).
sigue(oceanía, terminal_aérea, línea_5).
sigue(terminal_aérea, hangares, línea_5).
sigue(hangares, pantitlán, línea_5).

%-----------------------------------------------------
% Línea 6: El Rosario _ Martín Carrera
%-----------------------------------------------------
sigue(el_rosario, tezozómoc, línea_6).
sigue(tezozómoc, azcapotzalco, línea_6).
sigue(azcapotzalco, ferrería, línea_6).
sigue(ferrería, norte_45, línea_6).
sigue(norte_45, vallejo, línea_6).
sigue(vallejo, instituto_del_petróleo, línea_6).
sigue(instituto_del_petróleo, lindavista, línea_6).
sigue(lindavista, deportivo_18_de_marzo, línea_6).
sigue(deportivo_18_de_marzo, la_villa, línea_6).
sigue(la_villa, martín_carrera, línea_6).

%-----------------------------------------------------
% Línea 7: El Rosario _ Barranca del Muerto
%-----------------------------------------------------
sigue(el_rosario, aquiles_serdán, línea_7).
sigue(aquiles_serdán, camarones, línea_7).
sigue(camarones, refinería, línea_7).
sigue(refinería, tacuba, línea_7).
sigue(tacuba, san_joaquín, línea_7).
sigue(san_joaquín, polanco, línea_7).
sigue(polanco, auditorio, línea_7).
sigue(auditorio, constituyentes, línea_7).
sigue(constituyentes, tacubaya, línea_7).
sigue(tacubaya, san_pedro_de_los_pinos, línea_7).
sigue(san_pedro_de_los_pinos, san_antonio, línea_7).
sigue(san_antonio, mixcoac, línea_7).
sigue(mixcoac, barranca_del_muerto, línea_7).

%-----------------------------------------------------
% Línea 8: Garibaldi_Lagunilla _ Constitución de 1917
%-----------------------------------------------------
sigue(garibaldi_lagunilla, bellas_artes, línea_8).
sigue(bellas_artes, san_juan_de_letrán, línea_8).
sigue(san_juan_de_letrán, salto_del_agua, línea_8).
sigue(salto_del_agua, doctores, línea_8).
sigue(doctores, obrera, línea_8).
sigue(obrera, chabacano, línea_8).
sigue(chabacano, la_viga, línea_8).
sigue(la_viga, santa_anita, línea_8).
sigue(santa_anita, coyuya, línea_8).
sigue(coyuya, iztacalco, línea_8).
sigue(iztacalco, apatlaco, línea_8).
sigue(apatlaco, aculco, línea_8).
sigue(aculco, escuadrón_201, línea_8).
sigue(escuadrón_201, atlatilco, línea_8).
sigue(atlatilco, iztapalapa, línea_8).
sigue(iztapalapa, cerro_de_la_estrella, línea_8).
sigue(cerro_de_la_estrella, uam_1, línea_8).
sigue(uam_1, constitución_de_1917, línea_8).

%-----------------------------------------------------
% Línea 9: Tacubaya _ Pantitlán
%-----------------------------------------------------
sigue(tacubaya, patriotismo, línea_9).
sigue(patriotismo, chilpancingo, línea_9).
sigue(chilpancingo, centro_médico, línea_9).
sigue(centro_médico, lázaro_cárdenas, línea_9).
sigue(lázaro_cárdenas, chabacano, línea_9).
sigue(chabacano, jamaica, línea_9).
sigue(jamaica, mixiuhca, línea_9).
sigue(mixiuhca, velódromo, línea_9).
sigue(velódromo, ciudad_deportiva, línea_9).
sigue(ciudad_deportiva, puebla, línea_9).
sigue(puebla, pantitlán, línea_9).

%-----------------------------------------------------
% Línea A: Pantitlán _ La Paz
%-----------------------------------------------------
sigue(pantitlán, agrícola_oriental, línea_A).
sigue(agrícola_oriental, canal_de_san_juan, línea_A).
sigue(canal_de_san_juan, tepalcates, línea_A).
sigue(tepalcates, guelatao, línea_A).
sigue(guelatao, peñón_viejo, línea_A).
sigue(peñón_viejo, acatitla, línea_A).
sigue(acatitla, santa_marta, línea_A).
sigue(santa_marta, los_reyes, línea_A).
sigue(los_reyes, la_paz, línea_A).

%-----------------------------------------------------
% Línea B: Buenavista _ Ciudad Azteca
%-----------------------------------------------------
sigue(buenavista, guerrero, línea_B).
sigue(guerrero, garibaldi_lagunilla, línea_B).
sigue(garibaldi_lagunilla, lagunilla, línea_B).
sigue(lagunilla, tepito, línea_B).
sigue(tepito, morelos, línea_B).
sigue(morelos, san_lázaro, línea_B).
sigue(san_lázaro, flores_magón, línea_B).
sigue(flores_magón, romero_rubio, línea_B).
sigue(romero_rubio, oceanía, línea_B).
sigue(oceanía, deportivo_oceanía, línea_B).
sigue(deportivo_oceanía, bosque_de_aragón, línea_B).
sigue(bosque_de_aragón, villa_de_aragón, línea_B).
sigue(villa_de_aragón, nezahualcóyotl, línea_B).
sigue(nezahualcóyotl, impulsora, línea_B).
sigue(impulsora, río_de_los_remedios, línea_B).
sigue(río_de_los_remedios, múzquiz, línea_B).
sigue(múzquiz, ecatepec, línea_B).
sigue(ecatepec, olímpica, línea_B).
sigue(olímpica, plaza_aragón, línea_B).
sigue(plaza_aragón, ciudad_azteca, línea_B).

%============================================================
% OBSERVACIÓN:
%  No existen líneas 10 ni 11.  
%  Después de 9 siguen A y B y después sigue la línea 12...
%============================================================

%-----------------------------------------------------
% Línea 12: Mixcoac _ Tláhuac
%-----------------------------------------------------
sigue(mixcoac, insurgentes_sur, línea_12).
sigue(insurgentes_sur, hospital_20_de_noviembre, línea_12).
sigue(hospital_20_de_noviembre, zapata, línea_12).
sigue(zapata, parque_de_los_venados, línea_12).
sigue(parque_de_los_venados, eje_central, línea_12).
sigue(eje_central, ermita, línea_12).
sigue(ermita, mexicaltzingo, línea_12).
sigue(mexicaltzingo, atlatilco, línea_12).
sigue(atlatilco, culhuacán, línea_12).
sigue(culhuacán, san_andrés_tomatlán, línea_12).
sigue(san_andrés_tomatlán, lomas_estrella, línea_12).
sigue(lomas_estrella, calle_11, línea_12).
sigue(calle_11, periférico_oriente, línea_12).
sigue(periférico_oriente, tezonco, línea_12).
sigue(tezonco, olivos, línea_12).
sigue(olivos, nopalera, línea_12).
sigue(nopalera, zapotitlán, línea_12).
sigue(zapotitlán, tlaltenco, línea_12).
sigue(tlaltenco, tláhuac, línea_12).

%======= PREDICADOS DEL PROBLEMA  ==========================================================================================%

%======= Predicado para invocar el REPORTE SIMPLIFICADO ====================================================================%
reporte_simplificado(Inicio,Destino):- format("Calculando ruta, ten mucha paciencia :) ... ~n ~n"),
			%Pide la ruta más corta para comenzar con la redacción.
			ruta_corta(Inicio,Destino,Ruta),nth1(1,Ruta,Primer_Estación_G),sin_guión(Primer_Estación_G,Primer_Estación),
			format("Comenzar en la estación: ~w.  ~n",Primer_Estación),ruta_simplificado(Ruta,Trasbordos),
			última_estación(Trasbordos,Ruta,Destino),misma_línea(Trasbordos,Ruta,Destino).

%Auxiliar para la redacción del primer caso en el reporte simplificaado.
trasbordar_tomar(0,tomar).
trasbordar_tomar(X,Trasbordar):- X#>0,Trasbordar = 'trasbordar'.

última_estación(Trasbordos,Ruta,Destino):- Trasbordos#>0,length(Ruta,Tamaño),nth1(Tamaño,Ruta,Segunda),
	Tamaño2 #= Tamaño - 1,nth1(Tamaño2,Ruta,Primera),dame_línea(Primera,Segunda,Línea),color(Línea, Color_G),
	sin_guión(Color_G,Color),sin_línea(Línea,Número),dirección(Primera,Segunda,Dirección_G),sin_guión(Dirección_G,Dirección),
	format("Tomar línea ~w(~w) con dirección hacia ~w ~n hasta estación ~w. ~n ~n ",[Color,Número,Dirección,Destino]).

última_estación(Trasbordos,_,_):- Trasbordos#=0, format("").

%Cuando la ruta no tiene trasbordos solo imprime una instrucción.
misma_línea(Trasbordos,Ruta,Destino):- Trasbordos#=0,nth0(0,Ruta,Primera),nth0(1,Ruta,Segunda),
	dame_línea(Primera,Segunda,Línea),color(Línea, Color_G),sin_guión(Color_G,Color),
	sin_línea(Línea,Número),dirección(Primera,Segunda,Dirección_G),sin_guión(Dirección_G,Dirección),
	format("Tomar línea ~w(~w) con dirección hacia ~w ~n hasta estación ~w. ~n ~n ",[Color,Número,Dirección,Destino]).
	
misma_línea(Trasbordos,_,_):- Trasbordos#>0,format("").
		
ruta_simplificado(Ruta,Trasbordos):- aux_simplificado(Ruta,0,_,0,Trasbordos),!.

%Caso base de la recursión: Se detiene al llegar al penúltimo elemento de la lista.
aux_simplificado(Ruta,Incremento,Incremento,Trasbordos,Trasbordos):- length(Ruta,Tamaño), Incremento #=Tamaño-2,!.

%Recorre la ruta.
aux_simplificado(Ruta,Incremento,IncrementoT,Trasbordos,TrasbordosT):- Incremento2 #= Incremento + 1, Incremento3 #= Incremento + 2,
   nth0(Incremento,Ruta,Anterior),nth0(Incremento3,Ruta,Siguiente),nth0(Incremento2,Ruta,Actual_G),sin_guión(Actual_G,Actual),
   (				
	trasbordo(Anterior,Siguiente) ->
	%Cuando encuentra un trasbordo imprime una instrucción.	   
	Trasbordos2#=Trasbordos + 1,	
        trasbordar_tomar(Trasbordos,Tomar),
	dame_línea(Anterior,Actual_G,Línea),color(Línea, Color_G),sin_guión(Color_G,Color),
	sin_línea(Línea,Número),dirección(Anterior,Actual_G,Dirección_G),sin_guión(Dirección_G,Dirección),
	format("~w línea ~w(~w) con dirección hacia ~w ~n hasta estación ~w. ~n ~n ",[Tomar,Color,Número,Dirección,Actual]);											
	Trasbordos2#=Trasbordos
    ),
   aux_simplificado(Ruta,Incremento2,IncrementoT,Trasbordos2,TrasbordosT).

%Devuelve la dirección de dos estaciones contiguas, sin importar el sentido.
dirección(Anterior,Actual,Dirección):- sigue(Anterior,Actual,Línea),trayecto(Línea,_,Dirección).
dirección(Anterior,Actual,Dirección):- sigue(Actual,Anterior,Línea),trayecto(Línea,Dirección,_).

%Devuelve la línea de dos estaciones contiguas, sin importar el sentido.
dame_línea(Anterior,Actual,Línea):- sigue(Anterior,Actual,Línea).
dame_línea(Anterior,Actual,Línea):- sigue(Actual,Anterior,Línea).

%======= Predicado para invocar el REPORTE DE TIEMPO ====================================================================%
reporte_tiempo(Inicio,Destino):- format("Calculando ruta, ten mucha paciencia :) ... ~n"),
                valor_parámetro(tiempo_inicial, Tiempo_Inicial),format("Inicio: ~w minutos ~n",[Tiempo_Inicial]),
                %Pide la ruta más corta para comenzar a calcular el tiempo.
		ruta_corta(Inicio,Destino,Ruta), tiempo_ruta_reporte(Ruta,Tiempo),
		%Convierte el tiempo de formato minutos a horas.
		Horas #= Tiempo // 60, Minutos #= Tiempo - Horas * 60 ,
		format("Tiempo total de viaje:  ~w minutos =  ~w hora(s)  y ~w minutos ~n", [Tiempo,Horas,Minutos]).

%Devuelve el tiempo de una ruta.
tiempo_ruta_reporte(Ruta,Tiempo):- tiempo_aux_reporte(Ruta,0,_,0,Tiempo_Ruta),
	   length(Ruta,Tamaño),nth1(Tamaño,Ruta,Última),grado(Última,Grado),
	   valor_parámetro(tiempo_inicial, Tiempo_Inicial),valor_parámetro(tiempo_tramo, Tiempo_Tramo),
	   valor_parámetro(tiempo_final, Tiempo_Final), Tiempo_Auxiliar #= Tiempo_Tramo *Grado,
	   format("Final: ~w, ~w minutos + ~w minutos (Tiempo Final) ~n",[Última,Tiempo_Auxiliar,Tiempo_Final]),
	   %Obtiene el tiempo total del tramo.		   
	   Tiempo #= Tiempo_Inicial + Tiempo_Ruta + Tiempo_Tramo*Grado + Tiempo_Final.

%Caso base de la recursión: Se detiene al llegar al penúltimo elemento de la lista.
tiempo_aux_reporte(Ruta,Incremento,Incremento,Tiempo,Tiempo):- length(Ruta,Tamaño), Incremento #=Tamaño-2,!.

%Recorre la ruta.
tiempo_aux_reporte(Ruta,Incremento,IncrementoT,Tiempo,TiempoT):- Incremento2 #= Incremento + 1, Incremento3 #= Incremento + 2,
   nth0(Incremento,Ruta,Anterior),nth0(Incremento3,Ruta,Siguiente),nth0(Incremento2,Ruta,Actual),
   valor_parámetro(tiempo_tramo, Tiempo_Tramo),valor_parámetro(tiempo_transbordo, Tiempo_Trasbordo),
   grado(Actual,Grado),
   (			   		   
	trasbordo(Anterior,Siguiente) ->
	%Si se realizó un trasbordo, lo reporta y realiza la operación de tiempo.	
	Bandera_Trasbordo = 'trasbordo',Auxiliar_Tiempo#= Tiempo_Tramo * Grado + Tiempo_Trasbordo , Tiempo2#=Tiempo + Auxiliar_Tiempo;
	%Si no realizó un trasbordo, verifica si la estación es un trasbordo, lo reporta y realiza la operaciòn de tiempo.	
	(es_trasbordo(Actual)->Bandera_Trasbordo='sin trasbordo';Bandera_Trasbordo=' '), 
	Auxiliar_Tiempo#= Tiempo_Tramo * Grado , Tiempo2#=Tiempo + Auxiliar_Tiempo
	
   ),
   sin_guión(Anterior,Anterior_R),sin_guión(Actual,Actual_R),
   format("~w) ~w a ~w,  ~w  minutos, 	~w ~n",	[Incremento2,Anterior_R,Actual_R,Auxiliar_Tiempo,Bandera_Trasbordo]),
   tiempo_aux_reporte(Ruta,Incremento2,IncrementoT,Tiempo2,TiempoT).

%======= Predicado para invocar la MEJOR RUTA =============================================================================%
mejor_ruta(Inicio,Destino,Ruta):- format("Calculando ruta, ten mucha paciencia :) ... ~n"),
		ruta_corta(Inicio,Destino,Ruta), tiempo_ruta(Ruta,Tiempo),
		%Convierte el tiempo de formato minutos a horas.
		Horas #= Tiempo // 60,Minutos #= Tiempo - Horas * 60 ,
		format("T = ~w minutos =  ~w hora(s)  y ~w minutos ~n", [Tiempo,Horas,Minutos]).

%Calcula el tiempo de una ruta.
tiempo_ruta(Ruta,Tiempo):- tiempo_aux(Ruta,0,_,0,Tiempo_Ruta),
			   length(Ruta,Tamaño),nth1(Tamaño,Ruta,Última),grado(Última,Grado),
			   valor_parámetro(tiempo_inicial, Tiempo_Inicial),valor_parámetro(tiempo_tramo, Tiempo_Tramo),
			   valor_parámetro(tiempo_final, Tiempo_Final),	
			   Tiempo #= Tiempo_Inicial + Tiempo_Ruta + Tiempo_Tramo*Grado + Tiempo_Final.

%Caso base de la recursión: Se detiene al llegar al penúltimo elemento de la lista.
% [X,2,3,4,5,6,X] la razón: la condición para validar trasbordos usa un elemento anterior y uno posterior al actual, 
% el primer y el último elemento quedan descartados.
tiempo_aux(Ruta,Incremento,Incremento,Tiempo,Tiempo):- length(Ruta,Tamaño), Incremento #=Tamaño-2,!.

%Recorre la ruta.
%Incremento2 lleva el control de las iteraciones, Incremento e Incremento3 se usan como indices para acceder a elementos de la lista
tiempo_aux(Ruta,Incremento,IncrementoT,Tiempo,TiempoT):- Incremento2 #= Incremento + 1, Incremento3 #= Incremento + 2,
	   nth0(Incremento,Ruta,Anterior),nth0(Incremento3,Ruta,Siguiente),nth0(Incremento2,Ruta,Actual),
	   valor_parámetro(tiempo_tramo, Tiempo_Tramo),valor_parámetro(tiempo_transbordo, Tiempo_Trasbordo),
	   grado(Actual,Grado),
	   (					   
		trasbordo(Anterior,Siguiente) ->
		Tiempo2#=Tiempo + Tiempo_Tramo * Grado + Tiempo_Trasbordo;
		Tiempo2#=Tiempo + Tiempo_Tramo * Grado 
	   ),
	   %Tiempo2 lleva el valor de tiempo por iteración.
	   %TiempoT es el acumulador.
	   tiempo_aux(Ruta,Incremento2,IncrementoT,Tiempo2,TiempoT).

%======= Predicado para calcular la ruta con el menor costo ================================================================%	
%En una lista Costo-Ruta guardo las combinaciones que cumplen con las restricciones, hago uso de la propiedad de setof que ordena 
%los elementos y con member obtengo el primero (el de menor costo).
ruta_corta(Inicio,Destino,Ruta) :- setof(C-R,ruta_restricciones(Inicio,Destino,R,C,_), CostosRutas),
				   member(_-Ruta, CostosRutas),!.						

%Predicado que devuelve las rutas que cumplen con las restricciones.		
ruta_restricciones(Inicio, Destino, Ruta, Costo,Trasbordos) :- 
    ruta_aux(Inicio, Destino, [], 0, Ruta, Costo,0,Trasbordos),
    length(Ruta,Tamaño) , Tamaño #< 45,%45: No existe ruta óptima entre dos estaciones que necesite recorrer más de 45 estaciones.
    Costo #<700,%700: No existe ruta óptima cuyo costo sea mayor a 600 (con un margen de error muy alto).
    Trasbordos #<9.%9: No hay ruta óptima que necesite pasar por más de 9 estaciones que son trasbordos, OJO: no realizar trasbordos,
    		   %solamente pasar por ellas. (Si fueran trasbordos que se deben de hacer en la ruta, se necestian máximo 3 o 4).

%Estas restricciones dejan fuera opciones de rutas bastante absurdas.
    
%Ruta_Aux calcula la ruta y el costo (Es diferente del tiempo de los demás predicados).

%Recorre las posibilidades de estaciones contiguas hasta que llega a la estación destino.
ruta_aux(Destino, Destino, Visitadas, Costo, Ruta, Costo,Trasbordos, Trasbordos) :-
    reverse([Destino|Visitadas], Ruta).

ruta_aux(Inicio, Destino, Visitadas, Costo, Ruta, TotalCosto,Trasbordos, TotalTrasbordos) :-
    sigue(Inicio, Siguiente, _),
    %Valida que no es una estación que ya se visitó.
    \+ member(Siguiente, Visitadas),
    valor_parámetro(tiempo_tramo, Tiempo_Tramo),valor_parámetro(tiempo_transbordo, Tiempo_Trasbordo),
    grado(Siguiente,Grado),
    (
        %Calcula el costo de la ruta.
        %TotalCosto es el acumulador
        es_trasbordo(Inicio) ->
        Costo2#=Costo+Tiempo_Tramo*Grado+Tiempo_Trasbordo, Trasbordos2 #= Trasbordos + 1 ;
        Costo2#=Costo+Tiempo_Tramo*Grado, Trasbordos2 #= Trasbordos 
    ),
    ruta_aux(Siguiente, Destino, [Inicio|Visitadas], Costo2, Ruta, TotalCosto,Trasbordos2, TotalTrasbordos).

%El mismo predicado, pero con el orden de las estaciones invertido
%nos sirve para crear rutas en ambos sentidos.
ruta_aux(Inicio, Destino, Visitadas, Costo, Ruta, TotalCosto,Trasbordos, TotalTrasbordos) :-
    sigue(Siguiente, Inicio, _),
    \+ member(Siguiente, Visitadas),
    valor_parámetro(tiempo_tramo, Tiempo_Tramo),valor_parámetro(tiempo_transbordo, Tiempo_Trasbordo),
    grado(Siguiente,Grado),
    (
        es_trasbordo(Inicio) ->
        Costo2#=Costo + Tiempo_Tramo * Grado + Tiempo_Trasbordo ,Trasbordos2 #= Trasbordos + 1 ;
        Costo2#=Costo + Tiempo_Tramo * Grado ,Trasbordos2 #= Trasbordos 
    ),
    ruta_aux(Siguiente, Destino, [Inicio|Visitadas], Costo2, Ruta, TotalCosto,Trasbordos2, TotalTrasbordos).

%Verifica si una estación es un trasbordo.
es_trasbordo(Estacion) :- sigue(Estacion, _, Linea1),
                          sigue(_, Estacion, Linea2),
                          Linea1 \= Linea2.

%Posibles combinaciones en las que teniendo una estación intermedia, la estación anterior y posterior
%pueden indicar que se realizó un trasborde (En ambos sentidos, de ambas líneas).
trasbordo(Estacion1, Estacion2) :- 
    sigue(Intermedia, Estacion2, Linea2), 
    sigue(Estacion1, Intermedia, Linea1), 
    Linea1 \= Linea2.
    
trasbordo(Estacion1, Estacion2) :- 
    sigue(Intermedia, Estacion1, Linea2), 
    sigue(Estacion2, Intermedia, Linea1), 
    Linea1 \= Linea2.
    
trasbordo(Estacion1, Estacion2) :- 
    sigue(Intermedia, Estacion2, Linea2), 
    sigue(Intermedia, Estacion1, Linea1), 
    Linea1 \= Linea2.

trasbordo(Estacion1, Estacion2) :- 
    sigue(Intermedia, Estacion1, Linea2), 
    sigue(Intermedia, Estacion2, Linea1), 
    Linea1 \= Linea2.

trasbordo(Estacion1, Estacion2) :- 
    sigue(Estacion1, Intermedia, Linea1), 
    sigue(Intermedia, Estacion2, Linea2), 
    Linea1 \= Linea2.
 
trasbordo(Estacion1, Estacion2) :- 
    sigue(Estacion1, Intermedia, Linea1), 
    sigue(Estacion2, Intermedia, Linea2), 
    Linea1 \= Linea2.
    
trasbordo(Estacion1, Estacion2) :- 
    sigue(Estacion2, Intermedia, Linea1), 
    sigue(Intermedia, Estacion1, Linea2), 
    Linea1 \= Linea2.
    
trasbordo(Estacion1, Estacion2) :- 
    sigue(Estacion2, Intermedia, Linea1), 
    sigue(Estacion1, Intermedia, Linea2), 
    Linea1 \= Linea2. 
        
%Predicados auxiliares para calcular el grado de una estación.                        
grado(Estacion,Grado):- entran(Estacion,Lista1),salen(Estacion,Lista2), append([Lista1],Lista2,Lista_Estaciones),
			flatten(Lista_Estaciones,Estaciones),length(Estaciones,Grado).
			
entran(Estacion,Entran) :- findall(E,sigue(E,Estacion,_),Entran).

salen(Estacion,Salen) :- findall(E,sigue(Estacion,E,_),Salen).

%Predicados auxiliares para dar formato a los reportes.
sin_guión(Guión,Espacio):- atomic_list_concat(Lista,'_',Guión),
			       atomic_list_concat(Lista,' ',Espacio).	
					       
sin_línea(Constante,Número):- atomic_list_concat(Lista,'línea_',Constante),
      			      atomic_list_concat(Lista,'',Número).						       						

%----------------------------------------------


/*	OUTPUT

Prueba PANTITLÁN - CHILPANCINGO -----------------------------------------

?- reporte_tiempo(pantitlán,chilpancingo).
Calculando ruta, ten mucha paciencia :) ... 
Inicio: 8 minutos 
1) pantitlán a puebla,  10  minutos, 	  
2) puebla a ciudad deportiva,  10  minutos, 	  
3) ciudad deportiva a velódromo,  10  minutos, 	  
4) velódromo a mixiuhca,  10  minutos, 	  
5) mixiuhca a jamaica,  20  minutos, 	sin trasbordo 
6) jamaica a chabacano,  30  minutos, 	sin trasbordo 
7) chabacano a lázaro cárdenas,  10  minutos, 	  
8) lázaro cárdenas a centro médico,  20  minutos, 	sin trasbordo 
Final: chilpancingo, 10 minutos + 7 minutos (Tiempo Final) 
Tiempo total de viaje:  145 minutos =  2 hora(s)  y 25 minutos 
true.

?- reporte_simplificado(pantitlán,chilpancingo).
Calculando ruta, ten mucha paciencia :) ... 
Comenzar en la estación: pantitlán. 
 
Tomar línea café(9) con dirección hacia tacubaya 
 hasta estación chilpancingo.  
true .

?- mejor_ruta(pantitlán,chilpancingo,Ruta).
Calculando ruta, ten mucha paciencia :) ... 
T = 145 minutos =  2 hora(s)  y 25 minutos 
Ruta = [pantitlán, puebla, ciudad_deportiva, velódromo, mixiuhca, jamaica, chabacano, lázaro_cárdenas, centro_médico, chilpancingo].

----------------------------------------------------------------------------------------

Prueba MISTERIOS - NORTE 45 -----------------------------------------

?- mejor_ruta(misterios,norte_45,Ruta).
Calculando ruta, ten mucha paciencia :) ... 
T = 95 minutos =  1 hora(s)  y 35 minutos 
Ruta = [misterios, la_raza, autobuses_del_norte, instituto_del_petróleo, vallejo, norte_45].

?- reporte_tiempo(misterios,norte_45).
Calculando ruta, ten mucha paciencia :) ... 
Inicio: 8 minutos 
1) misterios a la raza,  20  minutos, 	sin trasbordo 
2) la raza a autobuses del norte,  10  minutos, 	  
3) autobuses del norte a instituto del petróleo,  30  minutos, 	trasbordo 
4) instituto del petróleo a vallejo,  10  minutos, 	  
Final: norte_45, 10 minutos + 7 minutos (Tiempo Final) 
Tiempo total de viaje:  95 minutos =  1 hora(s)  y 35 minutos 
true.


?- reporte_simplificado(misterios,norte_45).
Calculando ruta, ten mucha paciencia :) ... 
 
Comenzar en la estación: misterios.  
tomar línea amarillo(5) con dirección hacia politécnico 
hasta estación instituto del petróleo. 
 
Tomar línea rojo(6) con dirección hacia el rosario 
hasta estación norte_45.

*/



