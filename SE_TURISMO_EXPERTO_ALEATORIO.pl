%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Sistema que entrega el costo total del viaje a 
%%% realizar y te indica opciones adicionales de ciudades.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iniciar :-
  intro,
  reset_answers,
  hallar_destino(Destino), nl,
  describe_opciones(Destino), nl.

intro :-
  write('------------------------------------------------'),nl,
  write('BIENVENIDO A TU LUGAR IDEAL DE VIAJES EN HUANUCO'),nl,
  write('-----------------------------------------------'),nl,
  write('A continuacion, se mostraran una serie de preguntas. 
    Para responder NO coloque 0. y para SI coloque 1. 
    (colocar punto seguido luego del numero)'), nl, nl.

hallar_destino(Destino) :-
 destino(Destino),  !.

describe_opciones(Destino):-
 describe(Destino), !.

 % Store user answers to be able to track his progress
:- dynamic(progreso/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%           Reset                    %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
reset_answers :-
  retract(progreso(_, _)),
  fail.
reset_answers.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%           Reglas                   %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
destino(g1_pachitea):-
    provincia(pachitea),
    temperatura_altas(n_temp),
    lugar_historico(s_lug).

destino(g2_pachitea):-
    provincia(pachitea),
    temperatura_altas(n_temp),
    lugar_historico(n_lug),
    laguna(s_lagu).

destino(g3_pachitea):-
    provincia(pachitea),
    temperatura_altas(n_temp),
    lugar_historico(n_lug),
    laguna(n_lagu),
    catarata(s_cat).

destino(g4_pachitea):-
    provincia(pachitea),
    temperatura_altas(n_temp),
    lugar_historico(n_lug),
    laguna(n_lagu),
    catarata(n_cat).

destino(g5_pachitea):-
    provincia(pachitea),
    temperatura_altas(s_temp),
    catarata(s_cat).

destino(g6_pachitea):-
    provincia(pachitea),
    temperatura_altas(s_temp),
    catarata(n_cat).

destino(g1_leoncio):-
    provincia(leoncio_prado),
    catarata(s_cat).

destino(g2_leoncio):-
    provincia(leoncio_prado),
    catarata(n_cat),
    laguna(n_lagu).

destino(g1_humalies):-
    provincia(huamalies),
    lugar_historico(s_lug).

destino(g2_humalies):-
    provincia(huamalies),
    lugar_historico(n_lug),
    laguna(s_lagu).

destino(g3_humalies):-
    provincia(huamalies),
    lugar_historico(n_lug),
    laguna(n_lagu),
    catarata(s_cat).

destino(g4_humalies):-
    provincia(huamalies),
    lugar_historico(n_lug),
    laguna(n_lagu),
    catarata(n_cat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%           Reglas    Huanuco        %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
destino(g1_huanuco):-
    provincia(huanuco),
    temperatura_altas(s_temp),
    lugar_historico(s_lug).

destino(g2_huanuco):-
    provincia(huanuco),
    temperatura_altas(s_temp),
    lugar_historico(n_lug).

destino(g3_huanuco):-
    provincia(huanuco),
    temperatura_altas(n_temp),
    laguna(s_lagu),
    vivencial(s_viven). 

destino(g4_huanuco):-
    provincia(huanuco),
    temperatura_altas(n_temp),
    laguna(s_lagu),
    vivencial(n_viven).

destino(g5_huanuco):-
    provincia(huanuco),
    temperatura_altas(n_temp),
    laguna(n_lagu).

destino(g1_lauricocha):-
    provincia(lauricocha),
    lugar_historico(s_lug).

destino(g2_lauricocha):-
    provincia(lauricocha),
    lugar_historico(n_lug),
    laguna(s_lagu).    

destino(g3_lauricocha):-
    provincia(lauricocha),
    lugar_historico(n_lug),
    catarata(s_cat).

destino(g4_lauricocha):-
    provincia(lauricocha),
    lugar_historico(n_lug),
    catarata(n_cat).


/* COLOCAMOS ESTO AQUI PORQUE TIENE LA OPCION DE SELECCIONAR SI O NO EN TURISMO VIVENCIAL, PARA QUE NO GENERE ERROR*/
destino(g3_leoncio):-
    provincia(leoncio_prado),
    catarata(n_cat),
    laguna(s_lagu),
    vivencial(s_viven);
    vivencial(n_viven).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%             Preguntas              %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
preguntas(provincia) :-
  write('¿Que Provincia deseas Visitar?'), nl.

 preguntas(temperatura_altas) :-
  write('¿Te gustan los lugares con climas calidos y tropicales?'), nl.

preguntas(selva_sierra) :-
  write('¿Te interesaria conocer lugares de la selva?'), nl.
 
 preguntas(lugar_historico) :-
  write('¿Te gustaria visitar lugares historicos?'), nl.

 preguntas(laguna) :-
  write('¿Te gustaria conocer lagunas?'), nl.

 preguntas(catarata) :-
  write('¿Te gustaria conocer las cataratas?'), nl.

 preguntas(vivencial) :-
  write('¿Te gustaria hacer turismo vivencial?'), nl.  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%      Asignar respuestas            %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 provincia(Respuesta) :-
  progreso(provincia, Respuesta).  
 provincia(Respuesta) :-
  \+ progreso(provincia, _),
  ask(provincia, Respuesta, [huanuco, pachitea, leoncio_prado, huamalies, lauricocha]).

 temperatura_altas(Respuesta) :-
  progreso(temperatura_altas, Respuesta).  
 temperatura_altas(Respuesta) :-
  \+ progreso(temperatura_altas, _),
  ask(temperatura_altas, Respuesta, [n_temp, s_temp]).

 lugar_historico(Respuesta) :-
  progreso(lugar_historico, Respuesta).
 lugar_historico(Respuesta) :-
  \+ progreso(lugar_historico, _),
  ask(lugar_historico, Respuesta, [n_lug, s_lug]). 

   selva_sierra(Respuesta) :-
  progreso(selva_sierra, Respuesta).
 selva_sierra(Respuesta) :-
  \+ progreso(selva_sierra, _),
  ask(selva_sierra, Respuesta, [ n_selv, s_selv]).

catarata(Respuesta) :-
  progreso(catarata, Respuesta).
 catarata(Respuesta) :-
  \+ progreso(catarata, _),
  ask(catarata, Respuesta, [n_cat, s_cat]).

laguna(Respuesta) :-
  progreso(laguna, Respuesta).
 laguna(Respuesta) :-
  \+ progreso(laguna, _),
  ask(laguna, Respuesta, [n_lagu, s_lagu]).

   vivencial(Respuesta) :-
  progreso(vivencial, Respuesta).
 vivencial(Respuesta) :-
  \+ progreso(vivencial, _),
  ask(vivencial, Respuesta, [n_viven, s_viven]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    Lista de alternativas de las preguntas %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    respuesta(huanuco) :- 
        provincias(huanuco, P),
        write(P).
    respuesta(pachitea) :-
        provincias(pachitea, F),
        write(F).
    respuesta(leoncio_prado) :-
        provincias(leoncio_prado, G),
        write(G). 
    respuesta(huamalies) :-
        provincias(huamalies, H),
        write(H). 
    respuesta(lauricocha) :-
        provincias(lauricocha, I),
        write(I).

    respuesta(s_temp) :-
    write('Sí, me encanta un clima tropical'), nl.

    respuesta(n_temp) :-
    write('No, me gustan las temperaturas bajas'). 

    respuesta(s_lug) :-
    write('Sí, prefiero conocer lugares y centros históricos'), nl.
    
    respuesta(n_lug) :-
    write('No, disfruto caminar al aire libre'). 
    
    respuesta(s_selv) :-
    write('Sí, me encantaría conocer lugares de la selva'), nl.

    respuesta(n_selv) :-
    write('No, me encantaria conocer lugares de la sierra').
    
    respuesta(s_cat) :-
    write('Sí, me encantan las cataratas'), nl.

    respuesta(n_cat) :-
    write('No, no me gustan las cataratas').

    respuesta(s_lagu) :-
    write('Sí, quiero conocer lagunas'), nl.

    respuesta(n_lagu) :-
    write('No, no me gustan las lagunas').

    respuesta(s_viven) :-
    write('Sí, quiero eperimentar esa experiencia'), nl.

    respuesta(n_viven) :-
    write('No, quiero hacer recorridos largos y conocer paisajes escondidos').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%  Mostrar lista de alternativas        %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % [First|Rest] is the Choices list, Index is the index of First in Choices 
  alternativas([], _).
  alternativas([First|Rest], Index) :-
  write(Index), write(' '), respuesta(First), nl,
  NextIndex is Index + 1,
  alternativas(Rest, NextIndex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%      Texto para dar la Respuestas        %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*  B: Es la descripcion del lugar Turistico
    D: Es el nombre de la provincia
    E: Es el nombre del Distrito*/

listing(random_member).
random:random_member(W, X) :-
       length(X, Y),
       Z is random(Y),
       nth0(Z, X, W).

describe(g1_pachitea) :- 
    random_member(a(Lugar_t), [a('his_ushno'), a('his_rupestre')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C), distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g2_pachitea) :- 
    random_member(a(Lugar_t), [a('lag_verde_pozo'), a('lag_san_m'), a('lag_pozo_pata'),a('lag_linda'),a('lag_pajchaj'),a('lag_yanacocha'),a('lag_huasca')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g3_pachitea) :- 
    random_member(a(Lugar_t), [a('cat_yanano'), a('cat_potrero')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g4_pachitea) :- 
    random_member(a(Lugar_t), [a('torre_jirka'), a('cueva_machay'),a('bosque_sanmarcos'),a('cerro_aukin'),a('cerro_apallakuy'),a('rio_charamayo'),a('ichu_yanuna')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g5_pachitea) :- 
    random_member(a(Lugar_t), [a('cat_miguel')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g6_pachitea) :- 
    write('Aún no contamos con lugares turisticos recomendados para las opciones que ingresaste'),nl,nl.

describe(g1_leoncio) :- 
    random_member(a(Lugar_t), [a('cat_derrepente'),a('cat_honolulo'),a('cat_palmas'),a('cat_ninfas'),a('cat_mantaro')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g2_leoncio) :- 
    random_member(a(Lugar_t), [a('balneario'),a('bella_durmiente'),a('cueva_lechuzas'),a('cueva_pavas')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g3_leoncio) :- 
    random_member(a(Lugar_t), [a('lag_milagros')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g1_humalies) :- 
    random_member(a(Lugar_t), [a('his_tiricay'),a('his_llacuy'),a('his_pumaj'),a('his_tantamayo'), a('his_pikash')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.
    
describe(g2_humalies) :- 
    random_member(a(Lugar_t), [a('lag_carpa'),a('lag_angel'),a('lag_gasha')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g3_humalies) :- 
    random_member(a(Lugar_t), [a('cat_huerga')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g4_humalies) :- 
    random_member(a(Lugar_t), [a('bosque_piedras')]),
    es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%       Describe lugares de huanuco         %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

describe(g1_huanuco) :-
  random_member(a(Lugar_t), [a('his_kotosh'), a('his_plaza_arm'), a('his_pillco_m')]),
  es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g2_huanuco) :-
  random_member(a(Lugar_t), [a('nhis_igl_s'), a('nhis_casa')]),
  es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g3_huanuco) :-
  random_member(a(Lugar_t), [a('lag_mancap'), a('lag_parq')]),
  es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g4_huanuco) :-
  random_member(a(Lugar_t), [a('lag_qill')]),
  es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g5_huanuco) :-
  random_member(a(Lugar_t), [a('carp_punta'), a('sp_mach'), a('bn_coqui')]),
  es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%      Describe lugares de lauricocha       %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

describe(g1_lauricocha) :-
  random_member(a(Lugar_t), [a('ru_chic'), a('tor_piedra')]),
  es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g2_lauricocha) :-
  random_member(a(Lugar_t), [a('p_inc'), a('lag_lau'), a('ag_term')]),
  es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g3_lauricocha) :-
  random_member(a(Lugar_t), [ a('cat_cay')]),
  es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

describe(g4_lauricocha) :-
  random_member(a(Lugar_t), [a('co_hua')]),
  es_lugar(Lugar_t, A, B), es_distrito(A, C),distrito(A,D), provincias(C, E),
    write('Una buena opcion para visitar es:'),nl,
    write('Lugar Turistico: '), write(B), nl,
    write('Distrito: '), write(D),nl,
    write('Provincia: '), write(E), nl,nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Parses an Index and returns a Response representing the "Indexth" element in
%  Choices (the [First|Rest] list)
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  parse(0, [First|_], First).
  parse(Index, [First|Rest], Response) :-
  Index > 0,
  NextIndex is Index - 1,
  parse(NextIndex, Rest, Response).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Leer y guardar la alternativa seleccionada   %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	  
% Asks the Question to the user and saves the Answer

  ask(Preguntas, Respuesta, Choices) :-
  preguntas(Preguntas),
  alternativas(Choices, 0),
  read(Index),
  parse(Index, Choices, Response),
  asserta(progreso(Preguntas, Response)),
  Response = Respuesta.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%    BASE DE CONOCIMIENTO   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    provincias(pachitea, 'Pachitea').
    provincias(leoncio_prado, 'Leoncio Prado').
    provincias(huamalies, 'Huamalies'). 
    provincias(huanuco, 'Huánuco').
    provincias(lauricocha, 'Lauricocha').

    distrito(chaglla, 'Chaglla').
    distrito(umari, 'Umari').
    distrito(panao, 'Panao').
    distrito(molino, 'Molino').
    distrito(daniel, 'Daniel Alomia Robles').
    distrito(mariano, 'Mariano Damaso Beraun').
    distrito(rupa, 'Rupa Rupa (Tingo Maria)').
    distrito(llata, 'Llata').
    distrito(miraflores, 'Miraflores').
    distrito(tantamayo, 'Tantamayo').
    distrito(chavin, 'Chavin de Pariarca').

    distrito(huanuco, 'Huánuco').
    distrito(amarilis, 'Amarilis').
    distrito(pillco_marca, 'Pillco Marca').
    distrito(chinchao, 'Chinchao').


    distrito(queropalca, 'Queropalca').
    distrito(cauri, 'San Miguel de Cauri').
    distrito(jesus, 'Jesus').
    distrito(rondos, 'Rondos').
    distrito(banos, 'Baños').

    es_distrito(chaglla,pachitea). /* chaglla es distrito de pachitea*/
    es_distrito(umari,pachitea).
    es_distrito(panao,pachitea).
    es_distrito(molino,pachitea).
    es_distrito(daniel,leoncio_prado).
    es_distrito(mariano,leoncio_prado).
    es_distrito(rupa,leoncio_prado).
    es_distrito(llata,huamalies).
    es_distrito(miraflores,huamalies).
    es_distrito(tantamayo,huamalies).
    es_distrito(chavin,huamalies).

    es_distrito(huanuco,huanuco).
    es_distrito(amarilis,huanuco).
    es_distrito(pillco_marca,huanuco).
    es_distrito(chinchao,huanuco).

    es_distrito(queropalca, lauricocha).
    es_distrito(cauri, lauricocha).
    es_distrito(jesus, lauricocha).
    es_distrito(rondos, lauricocha).
    es_distrito(banos, lauricocha).


    /** es_lugar(lugar_turistico,distrito al que pertenece el lugar turistico, 'Nombre y descripcion del lugar turistico').**/
    es_lugar(lag_verde_pozo,chaglla, 'Laguna Verde Pozo').
    es_lugar(torre_jirka,chaglla, 'Torre Jirka').
    es_lugar(cueva_machay,chaglla, 'Cueva de las Lechuzas Machay').
    es_lugar(cat_yanano,chaglla, 'Catarata Yanano').
    es_lugar(cat_miguel,chaglla, 'Catarata San Miguel').
    es_lugar(cat_potrero,umari, 'Cataratas y Bosque Monte Potrero').
    es_lugar(bosque_sanmarcos,umari, 'Bosque San Marcos').
    es_lugar(his_ushno,umari, 'Centro Arqueológico Ushno'). 
    es_lugar(his_rupestre,umari, 'Pinturas Rupestres Letra Machay').
    es_lugar(lag_san_m,umari, 'Laguna San Marcos').
    es_lugar(cerro_aukin,umari, 'Cerro Aukin Goto').
    es_lugar(lag_pozo_pata, molino, 'Laguna Pozo Pata').
    es_lugar(lag_linda,molino, 'Laguna Linda Linda').
    es_lugar(lag_pajchaj,molino, 'Laguna Pajchaj').
    es_lugar(cerro_apallakuy,molino, 'Cerro Apallakuy').
    es_lugar(lag_yanacocha,panao, 'Laguna Yanacocha').
    es_lugar(lag_huasca,panao, 'Laguna Huascapampa').
    es_lugar(rio_charamayo,panao, 'Río Charamayo').
    es_lugar(ichu_yanuna,panao, 'Pueblo de Ichu Yanuna').
    es_lugar(lag_milagros,daniel, 'Laguna De Los Milagros').
    es_lugar(cat_derrepente,mariano, 'Catarata Derrepente').
    es_lugar(cat_honolulo,mariano, 'Catarata San Miguel de Honolulo').
    es_lugar(cat_palmas,mariano, 'Cascada El Encanto de las Palmas').
    es_lugar(cat_ninfas,mariano, 'Cascada Velo de las Ninfas').
    es_lugar(balneario,mariano, 'Balneario Las Alcantarillas').
    es_lugar(bella_durmiente,rupa, 'La Bella Durmiente').
    es_lugar(cueva_lechuzas,rupa, 'La Cueva de las Lechuzas').
    es_lugar(cueva_pavas,rupa, 'La Cueva de las Pavas').
    es_lugar(cat_mantaro,rupa, 'Cascada Poza del Mantaro').
    es_lugar(cat_huerga,llata, 'Cascada Huerga').
    es_lugar(his_tiricay,llata, 'Puente Tiricay').
    es_lugar(his_llacuy,llata, 'Sitio Arqueologico Llacuy').
    es_lugar(his_pumaj,miraflores, 'Castillo de Pumaj Jirca').
    es_lugar(bosque_piedras,miraflores, 'Bosque de Piedras Huahuan Apay').
    es_lugar(his_tantamayo,tantamayo, 'Racacielos de Tantamayo').
    es_lugar(lag_carpa,tantamayo, 'Laguna de Carpa').
    es_lugar(lag_angel,tantamayo, 'Laguna Angel Cocha').
    es_lugar(lag_gasha,chavin, 'Laguna Gasha Cocha').
    es_lugar(his_pikash,chavin, 'Sitio Arqueologico Pikash').


%%%%% HUANUCO %%%%
    es_lugar(his_kotosh, huanuco,'Las Manos Cruzadas de Kotosh').
    es_lugar(his_plaza_arm, huanuco,'Plaza de armas de huanuco').
    es_lugar(nhis_igl_s, huanuco,'Iglesia San Cristobal').

    es_lugar(casa_shis, amarilis,'Casa Asienda de Shishmay').
    es_lugar(nhis_casa, amarilis,'Casa Asienda Fundo Pacan').
    es_lugar(bn_coqui, amarilis,'Bosque nublado de choquecocha').
    es_lugar(lag_mancap, amarilis,'Laguna Manca Pozo').
    es_lugar(lag_parq, amarilis,'Laguna Parquencho').

    es_lugar(lag_qill, chinchao,'Laguna de Quilla Cocha').
    es_lugar(sp_mach, chinchao,'San Pedro de Machai').
    es_lugar(carp_punta, chinchao,'Carpish Punta').

    es_lugar(his_pillco_m, pillco_marca,'Pillco Mozo').


%%%%% LAURICOCHA %%%%
    es_lugar(p_inc, cauri, 'Puente Inca').
    es_lugar(cat_cay, cauri, 'Catarata Cayo').

    es_lugar(lag_carh, queropalca, 'Laguna Caruacocha').
    es_lugar(co_hua, queropalca, 'Cordillera de Huaylash').

    es_lugar(lag_lau, jesus, 'Laguna de Lauricocha').
    es_lugar(ru_chic, jesus, 'Ruinas de Chiquia').

    es_lugar(tor_piedra, rondos, 'La Tortuga de Piedra').
    es_lugar(ag_term, banos, 'Aguas Termales').
