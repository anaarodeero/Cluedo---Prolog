:- module(_,_,[classic,assertions,rtchecks]).

:-use_module(library(random)).
:-use_module(library(random_aggregates)).
:-use_module(library(idlists)).
:-use_module(library(lists)).
:-use_package(nativeprops).
:- use_module(library(llists)).
:- use_module(library(terms)).

sospechoso(rubio).
sospechoso(amapola).
sospechoso(prado).
sospechoso(orquidea).
sospechoso(celeste).
sospechoso(mora).

arma(candelabro).
arma(punal).
arma(tuberia_plomo).
arma(pistola).
arma(cuerda).
arma(herramienta).

habitacion(sala_baile).
habitacion(sala_billar).
habitacion(invernadero).
habitacion(comedor).
habitacion(vestibulo).
habitacion(cocina).
habitacion(biblioteca).
habitacion(salon).
habitacion(estudio).

carta(X) :- sospechoso(X).
carta(X) :- arma(X).
carta(X) :- habitacion(X).


:- dynamic dealer_cartas_jug/2.
:- dynamic cartas_si_jug/2.
:- dynamic cartas_no_jug/2.
:- dynamic solucion/3.
:- dynamic partida_si_jug/2.
:- dynamic sospechoso_acusacion_final/1.
:- dynamic arma_acusacion_final/1.
:- dynamic habitacion_acusacion_final/1.


reset_partida :-
    retractall(dealer_cartas_jug(_,_)),
    retractall(cartas_si_jug(_,_)),
    retractall(cartas_no_jug(_,_)),
    retractall(solucion(_,_,_)),
    retractall(sospechoso_acusacion_final(_)),
    retractall(arma_acusacion_final(_)),
    retractall(habitacion_acusacion_final(_)).

reset_turno :-
    retractall(partida_si_jug(_,_)).

random_member(X, List) :-
    length(List, Len),
    Len > 0,
    random(1,Len,N),
    nth(N, List, X).

random_members(Num, X,Y) :- random_findall(Num, A, member(A, Y), X).

sol_sospechoso(X) :-
    findall(Y, sospechoso(Y), Xs),
    random_member(X,Xs).

sol_arma(X) :-
    findall(Y, arma(Y), Xs),
    random_member(X,Xs).

sol_habitacion(X) :-
    findall(Y, habitacion(Y), Xs),
    random_member(X,Xs).

find_solucion :- sol_sospechoso(X), sol_arma(Y), sol_habitacion(Z),
   assert(solucion(X,Y,Z)).

repartir_cartas(Num, Cartas, X) :- random_members(Num,X,Cartas).

% Resto de cartas sin las 3 cartas solucion
lista_cartas(X) :-
    solucion(Sos,Arma,Hab),
    findall(Y, (carta(Y), Y\=Sos, Y\=Arma, Y\=Hab), X).



repartir_cartas_jugadores:-
    lista_cartas(Cartas),
    repartir_cartas(6,Cartas,Jug1),
    subtract(Cartas, Jug1, Resto),
    repartir_cartas(6,Resto,Jug2),
    subtract(Resto, Jug2, Jug3),
    add_hechos(1,Jug1),
    add_hechos(2,Jug2),
    add_hechos(3,Jug3).


add_hechos(_,[]).
add_hechos(Num, [H|T]) :-
    assert(dealer_cartas_jug(Num,H)),
    add_hechos(Num, T).


add_cartas_si(_,[]).
add_cartas_si(Jug,[H|T]) :-
    \+cartas_si_jug(Jug,H),
    assert(cartas_si_jug(Jug,H)),
    add_cartas_si(Jug,T).
add_cartas_si(Jug,[_|T]) :-
    add_cartas_si(Jug,T).

add_cartas_no(_,[]).
add_cartas_no(Jug,[H|T]) :-
    \+cartas_no_jug(Jug,H),
    assert(cartas_no_jug(Jug,H)),
    add_cartas_no(Jug,T).
add_cartas_no(Jug,[_|T]) :-
    add_cartas_no(Jug,T).

seguir(X) :-
    X == 's'.
seguir(_) :- halt.

main:-
    reset_partida,
    find_solucion,
    nl,write('¿Quieres jugar otra partida? s/n: '),
    read(X),nl,
    seguir(X),
    repartir_cartas_jugadores,
    get_cartas_jug(1,Mias),
    write('Mis cartas: '), write(Mias),nl,
    main_bucle(0).

main_bucle(N) :-
    reset_turno,
    \+comprobar(N),
    write('Acusacion'),nl,
    hacer_acusacion(S,A,H),
    tienen_jugadores_cartas([S,A,H]),
    N1 is N+1,
    main_bucle(N1).

comprobar(N) :-
    sospechoso_acusacion_final(S),
    arma_acusacion_final(A),
    habitacion_acusacion_final(H),
    write('VAMOS A HACER LA ACUSACION FINAL'),nl,
    write(' - ACUSACION FINAL: '), write([S,A,H]),nl,
    comprobar_solucion(S,A,H),
    write('ACUSACION CORRECTA con un total de '), write(N), write(' acusaciones.'),nl,
    main.

%% Jug 2 tiene alguna de las cartas
tienen_jugadores_cartas(L) :-
    tiene_jug_cartas(2, L).

%% Jug 3 tiene alguna de las cartas
tienen_jugadores_cartas(L) :-
    add_cartas_no(2, L),
    tiene_jug_cartas(3, L).

%% Ninguno de los jugadores tiene ninguna
tienen_jugadores_cartas(L) :-
    add_cartas_no(2, L),
    add_cartas_no(3, L),
    L = [S,A,H],
    assert(sospechoso_acusacion_final(S)),
    assert(arma_acusacion_final(A)),
    assert(habitacion_acusacion_final(H)),
    write('Ninguno tiene ninguna'), nl,nl.


tiene_jug_cartas(Jug,[]) :-
    findall(Y, partida_si_jug(Jug,Y), X),
    length(X, N),
    N > 0,
    random_member(Carta, X),
    write('Jugador '), write(Jug), write(' nos enseña la carta: '), write(Carta), nl,nl,
    add_cartas_si(Jug, [Carta]).
tiene_jug_cartas(Jug, [C|T]) :-
    dealer_cartas_jug(Jug, C),
    assert(partida_si_jug(Jug, C)),
    tiene_jug_cartas(Jug, T).
tiene_jug_cartas(Jug, [_|T]) :-
    tiene_jug_cartas(Jug, T).

hacer_acusacion(S,A,H) :-
    ha(sospechoso,S),
    ha(arma,A),
    ha(habitacion,H).

aleatorio(Carta) :-
    \+dealer_cartas_jug(1,Carta),
    \+cartas_si_jug(_,Carta).

ningun_jug(X) :-
    cartas_no_jug(2,X),
    cartas_no_jug(3,X).
    
comprobar_solucion(Sos_s,Arma_s,Hab_s):-
    solucion(Sos, Arma, Hab),
    Sos == Sos_s,
    Arma == Arma_s,
    Hab == Hab_s.


get_cartas_jug(Num, Cartas) :-
    findall(Y, dealer_cartas_jug(Num,Y), Cartas).


tiene_carta(Jug, H) :-
    dealer_cartas_jug(Jug, H),
    assert(cartas_si_jug(Jug,H)).

tiene_carta(Jug, H) :-
    assert(cartas_no_jug(Jug,H)).


head([H|_], H).

guardar_acusacion(Tipo,S) :-
    Tipo='sospechoso',
    assert(sospechoso_acusacion_final(S)).

guardar_acusacion(Tipo,S) :-
    Tipo='arma',
    assert(arma_acusacion_final(S)).

guardar_acusacion(_,S) :-
    assert(habitacion_acusacion_final(S)).




%% 0 ya se ha averiguado antes
ha(Tipo,S) :-
    A = '_acusacion_final',
    atom_concat([Tipo,A], Pred),
    call(Pred, S),
    write(' - 0: '), write(S),nl.

%% 1.1 ningun jugador la tiene
ha(Tipo,S) :-
    findall(Y, (call(Tipo,Y), ningun_jug(Y)), X),
    length(X,N),
    N == 1,
    head(X,S),
    guardar_acusacion(Tipo,S),
    write(' - 1.1: '),write(S),nl.

%% 1.2 descarte
ha(Tipo,S) :-
    findall(Y, (call(Tipo,Y), \+cartas_si_jug(_,Y)), X),
    length(X,N),
    N == 1,
    head(X,S),
    guardar_acusacion(Tipo,S),
    write(' - 1.2: '), write(S),nl.

%% 2 escoger alguna carta que se sepa que no la tiene un jug
ha(Tipo,S) :-
    findall(Y, (call(Tipo,Y), cartas_no_jug(_,Y), \+cartas_si_jug(_,Y)), X),
    length(X,N),
    N > 0,
    random_member(S,X),
    write(' - 2: '),write(S),nl.

%% 3 aleatoriamente
%% - no son mias
%% - no son ya solucion
%% - no la tienen otros jugadores
ha(Tipo,S) :-
    findall(Y, (call(Tipo,Y), aleatorio(Y)), X),
    length(X,N),
    N > 0,
    random_member(S,X),
    write(' - 3: '),write(S),nl.

