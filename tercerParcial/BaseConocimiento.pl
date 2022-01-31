% base de conocimiento para trabajar con los predicados

%=======================================
% Definicion de la base de conocimiento
%=======================================
padre(bryan,bill).
padre(bryan,david).
padre(david,alexis).
padre(david,kevin).
padre(bill,kely).
padre(bill,valeria).
padre(valeria,vaneza).
padre(vaneza,brandon).
padre(vaniza,noel).


%===========================
% Base de conocimiento del metro
%Declaracion de los colores 
%===========================
color(linea_1,rosa).
color(linea_2,azul_marino).
color(linea_3,verde_oscuro).
color(linea_4,azul_pastel).
color(linea_5,amarillo_canario).
color(linea_6, rojo).
color(linea_7, naranja ).
color(linea_8, verde).
color(linea_9,marron).
color(linea_A,morado).
color(linea_B,gris_verde).
color(linea_12, dorado).


%---------------------------------
%Linea 1: Observatorio-Pantitlan
%---------------------------------
sigue(observatorio, tacubaya, linea_1).
sigue(tacubaya, juanacatlan, linea_1).
sigue(juanacatlan, chapultepec, linea_1).
sigue(chapultepec, sevilla, linea_1).
sigue(sevilla, insurgentes, linea_1).
sigue(insurgentes, cuauhtemoc, linea_1).
sigue(cuauhtemoc, balderas, linea_1).
sigue(balderas, salto_del_agua, linea_1).
sigue(salto_del_agua, isabel_la_catolica, linea_1).
sigue(isabel_la_catolica, pino_suarez, linea_1).
sigue(pino_suarez, merced, linea_1).
sigue(merced, candelaria, linea_1).
sigue(candelaria, san_lazaro, linea_1).
sigue(san_lazaro, moctezuma, linea_1).
sigue(moctezuma, balbuena, linea_1).
sigue(balbuena, boulevard_pto_aereo, linea_1).
sigue(boulevard_pto_aereo, gomez_farias, linea_1).
sigue(gomez_farias, zaragoza, linea_1).
sigue(zaragoza, pantitlan, linea_1).

%---------------------------------
%Linea 2: Cuatro caminos-Tasquena
%---------------------------------
sigue(cuatro_caminos, panteones, linea_2).
sigue(panteones, tacuba, linea_2).
sigue(cuitlahuac, popotla, linea_2).
sigue(popotla, colegio_militar, linea_2).
sigue(colegio_militar, normal, linea_2).
sigue(normal, san_cosme, linea_2).
sigue(san_cosme, revolucion, linea_2).
sigue(revolucion, hidalgo, linea_2).
sigue(hidalgo, bellas_artes, linea_2).
sigue(bellas_artes, allende, linea_2).
sigue(allende, zocalo, linea_2).
sigue(zocalo, pino_suarez, linea_2).
sigue(pino_suarez, san_antonio_abad, linea_2).
sigue(san_antonio_abad, chabacano, linea_2).
sigue(chabacano, viaducto, linea_2).
sigue(viaducto, xola, linea_2).
sigue(xola, villa_de_cortes, linea_2).
sigue(villa_de_cortes, nativitas, linea_2).
sigue(nativitas, portales, linea_2).
sigue(portales, ermita, linea_2).
sigue(ermita, general_anaya, linea_2).
sigue(general_anaya, tasquena, linea_2).

%---------------------------------
%Linea 3: Indios Verdes-Universidad
%---------------------------------
sigue(indios_verdes, deportivo_18_de_marzo, linea_3).
sigue(deportivo_18_de_marzo, potrero, linea_3).
sigue(potrero, la_raza, linea_3).
sigue(la_raza, tlatelolco, linea_3).
sigue(tlatelolco, guerrero, linea_3).
sigue(guerrero, hidalgo, linea_3).
sigue(hidalgo, juarez, linea_3).
sigue(juarez, balderas, linea_3).
sigue(balderas, ninos_heroes, linea_3).
sigue(ninos_heroes, hospital_general, linea_3).
sigue(hospital_general, centro_medico, linea_3).
sigue(centro_medico, etiopia, linea_3).
sigue(etiopia, eugenia, linea_3).
sigue(eugenia, division_del_norte, linea_3).
sigue(division_del_norte, zapata, linea_3).
sigue(zapata, coyoacan, linea_3).
sigue(coyoacan, viveros, linea_3).
sigue(viveros, miguel_angel_de_quevedo, linea_3).
sigue(miguel_angel_de_quevedo, copilco, linea_3).
sigue(copilco, universidad, linea_3).

%---------------------------------
%Linea 4: Martin Carrera-Santa Anita
%---------------------------------
sigue(martin_carrera, talisman, linea_4).
sigue(talisman, bondojito, linea_4).
sigue(bondojito, consulado, linea_4).
sigue(consulado, canal_del_norte, linea_4).
sigue(canal_del_norte, morelos, linea_4).
sigue(morelos, candelaria, linea_4).
sigue(candelaria, fray_servando, linea_4).
sigue(fray_servando, jamaica, linea_4).
sigue(jamaica, santa_anita, linea_4).



%==============================
% Lineas del metro 5 
%=================================

sigue(politecnico, instituto_del_petroleo, linea_5).
sigue(instituto_del_petroleo, autobuses_nte,linea_5).
sigue(autobuses_nte,la_raza,linea_5).
sigue(la_raza,misterios,linea_5).
sigue(misterios,valle_gomez, linea_5).
sigue(valle_gomez,consulado,linea_5).
sigue(consulado,eduardo_molina,linea_5).
sigue(eduardo_molina,aragon,linea_5).
sigue(aragon,oceania,linea_5).
sigue(oceania,terminal_aerea,linea_5).
sigue(terminal_aerea,hangares,linea_5).
sigue(hangares,pantitlan,linea_5).

%========================
% linea del metro 6
%=========================

sigue(el_rosario,tezozomoc,linea_6).
sigue(tezozomoc, azcapotzalco,linea_6).
sigue(azcapotzalco, ferreria, linea_6).
sigue(ferreria,norte_45,linea_6).
sigue(norte_45,vallejo,linea_6).
sigue(vallejo, instituto_del_petroleo,linea_6).
sigue(instituto_del_petroleo,lindavista,linea_6).
sigue(lindavista,deportivo_18_de_marzo,linea_6).
sigue(deportivo_18_de_marzo,la_villa_basilica, linea_6).
sigue(la_villa_basilica,martin_carrera,linea_6).

%============================
% linea del metro 7
%============================

sigue(el_rosario,aquiles_serdan,linea_7).
sigue(aquiles_serdan,camarones,linea_7).
sigue(camarones,refineria,linea_7).
sigue(refineria, tacuba,linea_7).
sigue(tacuba,san_joaquin,linea_7).
sigue(san_joaquin, polanco,linea_7).
sigue(polanco,auditorio,linea_7).
sigue(auditorio,constituyentes,linea_7).
sigue(constituyentes,tacubaya,linea_7).
sigue(tacubaya, san_pedro_de_los_pinos,linea_7).
sigue(san_pedro_de_los_pinos,san_antonio,linea_7).
sigue(san_antonio,mixcoac,linea_7).
sigue(mixcoac,barranca_del_muerto,linea_7).

%===============================
% linea del metro 8
%===============================

sigue(garibaldi_lagunilla, bellas_artes,linea_8).
sigue(bellas_artes, san_juan_de_letran, linea_8).
sigue(san_juan_de_letran,salto_del_agua,linea_8).
sigue(salto_del_agua,doctores,linea_8).
sigue(doctores,obrera,linea_8).
sigue(obrera,chabacano,linea_8).
sigue(chabacano,la_viga,linea_8).
sigue(la_viga,santa_anita,linea_8).
sigue(santa_anita,coyuya,linea_8).
sigue(coyuya, iztacalco,linea_8).
sigue(iztacalco,apatlaco,linea_8).
sigue(apatlaco,aculco,linea_8).
sigue(aculco,escuadron_201,linea_8).
sigue(escuadron_201, atlalilco, linea_8).
sigue(atlalilco,iztapalapa,linea_8).
sigue(iztapalapa, cerro_de_la_estrella, linea_8).
sigue(cerro_de_la_estrella, uam_1, linea_8).
sigue(uam_1,constitucion_de_1917,linea_8).
%-----------------------------
%Linea 9: Tacubaya-Pantitlán
%-----------------------------
sigue(tacubaya, patriotismo, linea_9).
sigue(patriotismo, chilpancingo, linea_9).
sigue(chilpancingo, centro_medico, linea_9).
sigue(centro_medico, lazaro_cardenas, linea_9).
sigue(lazaro_cardenas, chabacano, linea_9).
sigue(chabacano, jamaica, linea_9).
sigue(jamaica, mixiuhca, linea_9).
sigue(mixiuhca, velodromo, linea_9).
sigue(velodromo, ciudad_deportiva, linea_9).
sigue(ciudad_deportiva, puebla, linea_9).
sigue(puebla, pantitlan, linea_9).

%---------------------------------
%Linea A: Pantitlán-La Paz
%---------------------------------
sigue(pantitlan, agricola_oriental, linea_A).
sigue(agricola_oriental, canal_de_san_juan, linea_A).
sigue(canal_de_san_juan, tepalcates, linea_A).
sigue(tepalcates, guelatao, linea_A).
sigue(guelatao, peñon_viejo, linea_A).
sigue(peñon_viejo, acatitla, linea_A).
sigue(acatitla, santa_marta, linea_A).
sigue(santa_marta, los_reyes, linea_A).
sigue(los_reyes, la_paz, linea_A).

%----------------------------------
%Linea B: Buenavista-Ciudad Azteca
%----------------------------------
sigue(buenavista, guerrero, linea_B).
sigue(guerrero, garibaldi_lagunilla, linea_B).
sigue(garibaldi_lagunilla, lagunilla, linea_B).
sigue(lagunilla, tepito, linea_B).
sigue(tepito, morelos, linea_B).
sigue(morelos, san_lazaro, linea_B).
sigue(san_lazaro, flores_magon, linea_B).
sigue(flores_magon, romero_rubio, linea_B).
sigue(romero_rubio, oceania, linea_B).
sigue(oceania, deportivo_oceania, linea_B).
sigue(deportivo_oceania, bosque_de_aragon, linea_B).
sigue(bosque_de_aragon, villa_de_aragon, linea_B).
sigue(villa_de_aragon, nezahualcoyotl, linea_B).
sigue(nezahualcoyotl, impulsora, linea_B).
sigue(impulsora, rio_de_los_remedios, linea_B).
sigue(rio_de_los_remedios, muzquiz, linea_B).
sigue(muzquiz, ecatepec, linea_B).
sigue(ecatepec, olimpica, linea_B).
sigue(olimpica, plaza_aragon, linea_B).
sigue(plaza_aragon, ciudad_azteca, linea_B).

%---------------------------
%Linea 12: Mixcoac-Tláhuac
%---------------------------
sigue(mixcoac, insurgentes_sur, linea_12).
sigue(insurgentes_sur, hospital_20_de_noviembre, linea_12).
sigue(hospital_20_de_noviembre, zapata, linea_12).
sigue(zapata, parque_de_los_venados, linea_12).
sigue(parque_de_los_venados, eje_central, linea_12).
sigue(eje_central, ermita, linea_12).
sigue(ermita, mexicaltzingo, linea_12).
sigue(mexicaltzingo, atlalilco, linea_12).
sigue(atlalilco, culhuacan, linea_12).
sigue(culhuacan, san_andres_tomatlan, linea_12).
sigue(san_andres_tomatlan, lomas_estrella, linea_12).
sigue(lomas_estrella, calle_11, linea_12).
sigue(calle_11, periferico_oriente, linea_12).
sigue(periferico_oriente, tezonco, linea_12).
sigue(tezonco, olivos, linea_12).
sigue(olivos, nopalera, linea_12).
sigue(nopalera, zapotitlan, linea_12).
sigue(zapotitlan, tlaltenco, linea_12).
sigue(tlaltenco, tlahuac, linea_12).
