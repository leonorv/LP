% Leonor Veloso 92509 
%--------- BEM-VINDX AO PROJETO DE LP ---------%
%--------- BIBLIOTECAS ---------%
:- use_module(library(clpfd)).
:- consult(codigo_comum).

%--------- FUNCOES AUXILIARES ---------%
% igual_a_um e igual_a_zero verificam se o argumento corresponde a 1 ou 0
igual_a_um(X) :- X == 1.
igual_a_zero(X) :- X == 0.

% Guarda o numero de 1s e 0s numa lista e em Var_a_meter guarda:
%   1, se houver 2 0s;
%   0, se houver 2 1s.
conta_num(L, Cont_1, Cont_0, Var_a_meter) :-
    include(igual_a_um, L, L_1), 
    length(L_1, Cont_1),
    include(igual_a_zero, L, L_0), 
    length(L_0, Cont_0),
    (Cont_0 == 2, Var_a_meter = 1
    ;
    Cont_1 == 2, Var_a_meter = 0
    ;
    Var_a_meter = _).

% Devolve true se 2 listas forem iguais, ignorando as vars anonimas.
listas_iguais_sem_var([], []).
listas_iguais_sem_var([X1|R1], [X2|R2]):-
    (X1 == X2; (var(X1), var(X2))) , listas_iguais_sem_var(R1, R2).

% Preenche as vars anonimas de uma lista com Num_a_preencher.
preenche([], [], _).
preenche([X | R], [NX | Res], Num_a_preencher) :-
        (var(X), NX = Num_a_preencher, preenche(R, Res, Num_a_preencher) 
        ;
        NX = X, preenche(R, Res, Num_a_preencher)).

% Devolve true se os argumentos forem listas iguais.
listas_iguais([], []).
listas_iguais([X1|R1], [X2|R2]):-
    X1 == X2 , listas_iguais(R1, R2).

% Devolve true se os argumentos forem matrizes iguais.
puzzles_iguais([], []).
puzzles_iguais([X | P1], [Y | P2]) :-
    listas_iguais_sem_var(X, Y), puzzles_iguais(P1, P2).

% Guarda em LFinal uma listas dos indices onde ha diferencas nas listas dadas como argumento, comecando num indice dado como argumento.
compara_linhas(_, [], [], LFinal, LFinal). 
compara_linhas(Indice, [X | L1], [Y | L2], Lista_Indices, Final) :-
    (((var(X), not(var(Y))); (var(Y), not(var(X)));(number(X), number(Y), X =\= Y)), 
    append(Lista_Indices, [Indice], N_L), N_Indice is Indice + 1, 
    compara_linhas(N_Indice, L1, L2, N_L, Final)
    ;
    N_Indice is Indice + 1, compara_linhas(N_Indice, L1, L2, Lista_Indices, Final)).

% Guarda em Final uma lista de coordenadas diferentes entre 2 matrizes, utilizando compara_linhas recursivamente, 
% comecando numa linha cujo numero e dado como argumento.
compara_puzzle_aux([], [], _, Final, Final).
compara_puzzle_aux([X | P1], [Y | P2], Linha, Lista_Coor, Final) :-
    (listas_iguais_sem_var(X, Y), N_Linha is Linha + 1, compara_puzzle_aux(P1, P2, N_Linha, Lista_Coor, Final)
    ;
    compara_linhas(1, X, Y, [], L_Col_Dif), 
    junta_coordenadas(Linha, L_Col_Dif, [], Coordenadas),
    append(Lista_Coor, Coordenadas, N_L), N_Linha is Linha + 1,
    compara_puzzle_aux(P1, P2, N_Linha, N_L, Final)).

% Aplica compara_puzzle_aux, iniciando a auxiliar na primeira lista.
compara_puzzle(P1, P2, Final) :- 
    compara_puzzle_aux(P1, P2, 1, [], Final).
    
% Cria uma lista de coordenadas no formato (L, C), dado o numero de uma linha e uma lista de colunas.
junta_coordenadas(_,[], Final, Final).
junta_coordenadas(Linha, [Col1 | Lista_Col], Coordenadas, Final) :-
    append(Coordenadas, [(Linha, Col1)], N_L),
    junta_coordenadas(Linha, Lista_Col, N_L, Final).

% Guarda em Pos a primeira posicao da matriz Puz que contem uma variavel anonima (livre).
pos_livre(Puz, Pos) :- mat_ref(Puz, Pos, El), var(El), !.

% Devolve true se o argumento for uma lista sem variaveis anonimas.
lista_sem_var([]).
lista_sem_var([X | L]) :-
    nonvar(X), lista_sem_var(L).

% Devolve true se o argumento for uma matriz sem variaveis anonimas.
puz_sem_var([]).
puz_sem_var([L1 | Puz]) :-
    lista_sem_var(L1), puz_sem_var(Puz).
    
%---------3.1.1---------% 
% Aplica a regra 1 a uma lista de 3 elementos.
aplica_R1_triplo([X, Y, Z], Res) :-
    L = [X, Y, Z], 
    conta_num(L, _, _, Var_a_meter), 
    (var(X), NX = Var_a_meter, Res = [NX, Y, Z]
    ;
    var(Y), NY = Var_a_meter, Res = [X, NY, Z]
    ;
    var(Z), NZ = Var_a_meter, Res = [X, Y, NZ]
    ;
    (not(X == Y); not(Y == Z); not(X == Z)), Res = L).

%---------3.1.2---------%
% Aplica a regra 1 a uma lista.
aplica_R1_fila_aux([X, Y | []], [X, Y | []]).
aplica_R1_fila_aux([X, Y, Z | R], [NX | N_Fila]) :-
    aplica_R1_triplo([X, Y, Z], [NX, NY, NZ]), 
    aplica_R1_fila_aux([NY, NZ | R], N_Fila).

%---------3.1.3---------%
% Aplica a regra 1 a uma lista, ate ja nao haver mais alteracoes possiveis.
aplica_R1_fila(Fila, Novissima_Fila) :-
    aplica_R1_fila_aux(Fila, N_Fila), !,
    (not(listas_iguais_sem_var(Fila, N_Fila)), aplica_R1_fila(N_Fila, Novissima_Fila)
    ;
    Novissima_Fila = N_Fila).

%---------3.1.4---------%
% Aplica a regra 2 a uma lista.
aplica_R2_fila(Fila, N_Fila) :-
    length(Fila, Len), N is div(Len, 2),
    conta_num(Fila, Cont_1, Cont_0, _),
    (Cont_0 == N, preenche(Fila, N_Fila, 1)
    ;
    Cont_1 == N, preenche(Fila, N_Fila, 0)
    ;
    Cont_1 < N, Cont_0 < N, N_Fila = Fila).

%---------3.1.5---------%  
% Aplica a regra 1 e a regra 2 a uma lista.
aplica_R1_R2_fila(Fila, Novissima_Fila) :-
    aplica_R1_fila(Fila, Nova_Fila), aplica_R2_fila(Nova_Fila, Novissima_Fila); fail.

%---------3.1.6---------%  
% Aplica a regra 1 e a regra 2 as linhas de uma matriz.
aplica_R1_R2_puzzle_linhas([], []).
aplica_R1_R2_puzzle_linhas([H | Res_Puz], [NH | N_Puz]) :- 
    aplica_R1_R2_fila(H, NH), 
    aplica_R1_R2_puzzle_linhas(Res_Puz, N_Puz).

% Aplica a regra 1 e a regra 2 as colunas de uma matriz.
aplica_R1_R2_puzzle_colunas([], []).
aplica_R1_R2_puzzle_colunas(Puz, N_Puz) :- 
    mat_transposta(Puz, Trans),
    aplica_R1_R2_puzzle_linhas(Trans, Res),
    mat_transposta(Res, N_Puz).

% Aplica as 2 funcoes anteriores para aplicar as regras 1 e 2 a uma matriz.
aplica_R1_R2_puzzle(Puz, Novissimo_Puz) :-
    aplica_R1_R2_puzzle_linhas(Puz, N_Puz),
    aplica_R1_R2_puzzle_colunas(N_Puz, Novissimo_Puz).

%---------3.1.7---------%  
% Inicializa um puzzle.
inicializa(Puz, Novissimo_Puz) :-
    aplica_R1_R2_puzzle(Puz, N_Puz),
    (not(puzzles_iguais(Puz, N_Puz)), inicializa(N_Puz, Novissimo_Puz)
    ;
    aplica_R1_R2_puzzle(N_Puz, Novissimo_Puz)).

%---------3.2---------%
% Recebe uma lista e uma matriz e devolve falso se essa lista for igual a alguma linha da matriz.
verifica_linha_puzzle(_, []).
verifica_linha_puzzle(L1, [L2 | Res_Puz]) :-
    not(listas_iguais(L1, L2)), verifica_linha_puzzle(L1, Res_Puz).

% Verifica se ha 2 linhas iguais numa matriz.
verifica_R3_linhas([]).
verifica_R3_linhas([L1, L2 | Puz]) :-
    verifica_linha_puzzle(L1, [L2 | Puz]), verifica_linha_puzzle(L2, Puz). 

% Verifica se ha 2 colunas iguais numa matriz.
verifica_R3_colunas([]).
verifica_R3_colunas(Puz) :-
    mat_transposta(Puz, Trans), verifica_R3_linhas(Trans).

% Aplica a regra R3 a um puzzle.
verifica_R3([]).
verifica_R3(Puz) :-
    verifica_R3_linhas(Puz), verifica_R3_colunas(Puz).

%---------3.3---------%
% Aplica a regra 1 e a regra 2 a linha e a coluna de uma posicao dada na forma de coordenada.
propaga_uma_posicao((L, C), Puz, Novissimo_Puz) :- 
    mat_transposta(Puz, Trans), mat_elementos_coluna(Trans, L, Els_Linha),
    mat_elementos_coluna(Puz, C, Els_Coluna), 
    aplica_R1_R2_fila(Els_Linha, N_Els_Linha), !,
    mat_muda_linha(Puz, L, N_Els_Linha, N_Puz),
    aplica_R1_R2_fila(Els_Coluna, N_Els_Coluna), !,
    mat_muda_coluna(N_Puz, C, N_Els_Coluna, Novissimo_Puz).

% Propaga as alteracoes feitas a uma posicao.
propaga_posicoes([], Puz, Puz). 
propaga_posicoes([P1 | Posicoes], Puz, Novissimo_Puz) :- 
    (propaga_uma_posicao(P1, Puz, N_Puz), !, 
    compara_puzzle(Puz, N_Puz, Lista_Coordenadas), !, 
    append(Lista_Coordenadas, Posicoes, Lista_Final), 
    propaga_posicoes(Lista_Final, N_Puz, Novissimo_Puz)
    ;
    fail).

%---------3.4---------%
% Recebe um puzzle por resolver e devolve a solucao.
resolve(Puz, Sol) :-
    inicializa(Puz, Puz_Inicializado),
    verifica_R3(Puz_Inicializado), 
    (puz_sem_var(Puz_Inicializado), Sol = Puz_Inicializado
    ;
    resolve_aux(Puz_Inicializado, Sol)).

% Aplica o algoritmo de solucao de puzzles binarios ate o puzzle estar preenchido.
resolve_aux(Puz, Puz_Resolvido) :- puz_sem_var(Puz), Puz_Resolvido = Puz.
resolve_aux(Puz, Puz_Resolvido) :-
    pos_livre(Puz, Pos_Livre),
    (mat_muda_posicao(Puz, Pos_Livre, 0, N_Puz), 
    propaga_posicoes([Pos_Livre], N_Puz, Puz_Propagado),  
    verifica_R3(Puz_Propagado), resolve_aux(Puz_Propagado, Puz_Resolvido)
    ;
    mat_muda_posicao(Puz, Pos_Livre, 1, N_Puz), 
    propaga_posicoes([Pos_Livre], N_Puz, Puz_Propagado), 
    verifica_R3(Puz_Propagado), resolve_aux(Puz_Propagado, Puz_Resolvido)).