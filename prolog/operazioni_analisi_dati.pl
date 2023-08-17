/* Programma Prolog per calcolare regressione lineare e k-nearest neighbors */

% ESECUZIONE MAIN

/* Punto iniziale del programma */
main :-
  nl, nl, 
  stampa_riga_orizzontale,
  write('Progetto del corso di Programmazione Logica e Funzionale'), nl,
  write('Anno 2022/2023'), nl,
  write('Progetto realizzato da: Andrea De Lorenzis'), nl, nl,
  menu_principale,
  stampa_riga_orizzontale.
  
/* Menu di selezione dell'operazione da svolgere */
menu_principale :-
  nl, write('Selezionare l\'operazione da svolgere: '), nl,
  write('1 - Regressione lineare'), nl,
  write('2 - K-Nearest Neighbors'), nl,
  write('3 - Esci'), nl,
  
  read(Scelta),
  (Scelta = 1 -> esegui_regressione_lineare, menu_principale ;
   Scelta = 2 -> esegui_knn, menu_principale ;
   Scelta = 3 -> nl, write('Chiusura del programma.'), nl ;
   write('Scelta incorretta. Per favore, seleziona una delle opzioni valide.'), nl,
   menu_principale). 

% CALCOLO REGRESSIONE LINEARE

/* Predicato per eseguire la regressione lineare */
esegui_regressione_lineare :-
  nl, write('-------- Regressione Lineare --------'), nl,
  leggi_dataset(Punti),
  nl, write('Adattamento della retta al dataset..'), nl,
  calcola_coefficienti_retta(Punti, Pendenza, Intercetta), 
  nl,
  format('Retta interpolatrice: y = ~2fx + ~2f~n', [Pendenza, Intercetta]),
  valuta_valori_x(Pendenza, Intercetta).

/* Predicato per leggere una lista di tuple da tastiera */
leggi_dataset(Lista) :-
  repeat,
  nl, write('Inserisci i punti del dataset nel formato: [(x1,y1), ..., (xn,yn)]:'), 
  nl, read(Lista),
  (lista_punti_valida(Lista) ->
     !    % Successo: lista valida di punti
     ;
     nl, write('Input invalido. La lista deve contenere almeno due punti '),
     nl, write('nel formato [(x,y), ...]'), nl,
     fail % Fallimento: formato non valido, ritentare
  ).

/* Predicato per validare una lista di punti 2D */
lista_punti_valida(Lista) :-
  is_list(Lista),
  length(Lista, NumPunti),
  NumPunti >= 2,
  forall(member(Punto, Lista), punto_valido(Punto)).
  
/* Predicato per controllare che un punto sia nel formato (x,y) */
punto_valido(Punto) :- 
  Punto = (X, Y),
  number(X),
  number(Y).

/* Calcola i coefficienti (pendenza e intercetta) della retta interpolatrice */
calcola_coefficienti_retta(Punti, Pendenza, Intercetta) :-
  estrai_coordinate_x(Punti, Xs),
  estrai_coordinate_y(Punti, Ys),
  varianza(Xs, VarX),
  covarianza(Xs, Ys, CovXY),
  Pendenza is CovXY / VarX,
  media(Ys, MediaY),
  media(Xs, MediaX),
  Intercetta is MediaY - Pendenza * MediaX.
 
/* Estra le coordinate X da una lista di punti */
estrai_coordinate_x([], []).
estrai_coordinate_x([(X, _) | Punti], [X | Xs]) :-
  estrai_coordinate_x(Punti, Xs).
  
/* Estrae le coordinate Y da una lista di punti */
estrai_coordinate_y([], []).
estrai_coordinate_y([(_, Y) | Punti], [Y | Ys]) :-
  estrai_coordinate_y(Punti, Ys).

/* Calcola la varianza di una lista di numeri */  
varianza([], 0).
varianza([X | Xs], Var) :-
  media([X | Xs], Media),
  varianza(Xs, Resto),
  Var is Resto + (X - Media) * (X - Media).
	
/* Calcola la covarianza di due liste di numeri */
covarianza([], [], 0).
covarianza([X | Xs], [Y | Ys], Cov) :-
  media([X | Xs], MediaX),
  media([Y | Ys], MediaY),
  covarianza(Xs, Ys, Resto),
  Cov is Resto + (X - MediaX) * (Y - MediaY).

/* Calcola la media di una lista di numeri */
media([], 0).
media([X | Xs], Media) :-
  media(Xs, Resto),
  length(Xs, N),
  Media is (X + N * Resto) / (N + 1).

/* Predicato che cicla continuamente per valutare nuovi valori di x */
valuta_valori_x(Pendenza, Intercetta) :-
  leggi_valore(X),
  valuta_valore(X, Pendenza, Intercetta, Y),
  nl, format('Per X = ~2f il valore previsto e\' Y = ~2f', [X, Y]), nl,
  richiedi_continuazione(Scelta),
  (Scelta = 's' -> valuta_valori_x(Pendenza, Intercetta) ; true).

/* Predicato che ottiene dall'utente una coordinata X da valutare sulla 
 * retta ottenuta */
leggi_valore(X) :-
  repeat,
  nl, write('Inserisci un valore per la coordinata x: '), nl,
  read(X),
  (number(X) ->
     true  % Successo: X è un numero
     ;
     nl, write('Il valore inserito non e\' un numero.'), nl,
     fail % Fallimento: X non è un numero, ritentare
  ).

/* Predicato che valuta la regressione lineare per il punto */
valuta_valore(X, Pendenza, Intercetta, Y) :-
  Y is Pendenza * X + Intercetta.

% CALCOLO K-NEAREST NEIGHBORS

/* Predicato per eseguire il KNN */
esegui_knn :- 
  nl, write('-------- K-Nearest Neighbors --------'), nl,
  leggi_dataset_etichettato(Dataset),
  length(Dataset, NumPunti),
  repeat,
  nl, write('Inserisci il valore per k: '), nl,
  read(K),
  (number(K) -> 
     K > 0,
    (K =< NumPunti ->
       !
       ; 
       nl, write('Input invalido. Inserisci un intero positivo '),
       write('minore del (o uguale al) numero totale di punti nel dataset'), nl,
       format('(Numero di punti: ~d)', [NumPunti]), nl,
       fail
    )
    ;
    nl, write('Input invalido. Inserisci un intero positivo.'), nl,
    fail
  ),
  valuta_punti(Dataset, K).

/* Predicato per ottenere dall'utente un dataset di punti etichettati con una classe */
leggi_dataset_etichettato(Lista) :-
  repeat,
  nl, write('Inserisci i punti del dataset nel formato: '),
  write('[(x1,y1,<classe>), ..., (xn,yn,<classe>)].'), nl,
  read(Lista),
  (dataset_etichettato_valido(Lista) ->
     !  % Successo: la lista è valida
     ;
     nl, write('Input invalido. La lista deve contenere almeno due punti '),
     write('nel formato [(x,y,<classe>), ...]'), nl,
     fail % Fallimento: formato lista invalido, ritentare
  ).
	
/* Predicato di validazione per un dataset con etichette */
dataset_etichettato_valido(Lista) :-
  is_list(Lista),
  length(Lista, NumPunti),
  NumPunti >= 2,
  forall(member(Punto, Lista), punto_etichettato_valido(Punto)).

/* Predicato per controllare se un elemento è un punto etichettato valido nel 
   formato (X, Y, C) */
punto_etichettato_valido(Punto) :- 
  Punto = (X, Y, C),
  number(X),
  number(Y),
  atom(C).

/* Predicato per inserire continuamente punti da valutare con KNN */
valuta_punti(_, 0) :- !.
valuta_punti(Dataset, K) :-
  leggi_punto_etichettato(Punto),
  trova_vicini(K, Punto, Dataset, Vicini),
  nl, format('I ~d vicini del punto sono: ', [K]), nl, 
  stampa_vicini(Vicini), nl, 
  trova_classe_maggioranza(Vicini, Classe),
  format('La classe prevista per il punto e\': ~w~n', [Classe]), 
  richiedi_continuazione(Scelta),
  (Scelta = 's' -> valuta_punti(Dataset, K) ; true).

/* Predicato per leggere un singolo punto da valutare con KNN */
leggi_punto_etichettato(Punto) :-
  repeat,
  nl, write('Inserisci il valore del punto da testare nel formato (x,y): '), nl,
  read(Punto),
  (punto_valido(Punto) ->
   true  % Successo: punto valido, uscita dal repeat
   ;
   nl, write('Punto non valido. Assicurati di inserire un punto '),
   write('nel formato (x,y).'), nl,
   fail % Fallimento: punto non valido, ritentare
  ).

/* Predicato che trova i k vicini per un punto */
trova_vicini(K, Punto, Dataset, Vicini) :-
  calcola_distanze(Punto, Dataset, Distanze),
  ordina_distanze(Distanze, DistanzeOrdinate),
  prendi(K, DistanzeOrdinate, CoppieVicini),
  converti_coppie_in_vicini(CoppieVicini, Vicini).

/* Predicato per calolcare le distanze tra PuntoTest e ogni altro punto 
 * del dataset */
calcola_distanze(_, [], []).
calcola_distanze(PuntoTest, [Punto | Resto], [Dist-Punto | Distanze]) :-
  distanza(Punto, PuntoTest, Dist),
  calcola_distanze(PuntoTest, Resto, Distanze).
  
/* Predicato che calcola la distanza euclidea tra due punti */
distanza((X1, Y1, _), (X2, Y2), Dist) :-
  Dist is sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)).

/* Ordina le distanze in ordine crescente */
ordina_distanze(Distanze, DistanzeOrdinate) :-
  keysort(Distanze, DistanzeOrdinate).

/* Prende i primi K elementi di una lista */
prendi(0, _, []).
prendi(K, [X | Xs], [X | Resto]) :-
  K > 0,
  K1 is K - 1,
  prendi(K1, Xs, Resto).
  
/* Rimuove il valore della distanza dal formato (Dist-(X, Y, Class)) 
   per ottenere il formato (X, Y, Class) */
converti_coppie_in_vicini([], []).
converti_coppie_in_vicini([_-Vicino | CoppieRestanti], [Vicino | ViciniRestanti]) :-
  converti_coppie_in_vicini(CoppieRestanti, ViciniRestanti).
  
/* Predicato per stampare i vicini nel formato (x, y, C) */
stampa_vicini([]).
stampa_vicini([(X, Y, C) | Resto]) :-
  format('Punto: (~2f, ~2f, ~w)~n', [X, Y, C]),
  stampa_vicini(Resto).
  
/* Predicato che trova la classe di maggioranza tra i vicini */
trova_classe_maggioranza(Vicini, ClasseMaggioranza) :-
  conta_occorrenze_classe(Vicini, MapConteggi),
  classe_max_occorrenze(MapConteggi, ClasseMaggioranza).

/* Predicato per conteggiare le occorrenze di ogni classe nei vicini */
conta_occorrenze_classe(Vicini, MapConteggi) :-
  conta_occorrenze_classe(Vicini, [], MapConteggi).

conta_occorrenze_classe([], MapConteggi, MapConteggi).
conta_occorrenze_classe([(_, _, Classe) | Vicini], MapConteggiParziale, MapConteggi) :-
  aggiorna_map_conteggi(Classe, MapConteggiParziale, NuovaMapConteggi),
  conta_occorrenze_classe(Vicini, NuovaMapConteggi, MapConteggi).

aggiorna_map_conteggi(Classe, [], [(Classe, 1)]).
aggiorna_map_conteggi(Classe, 
                      [(Classe, Conteggio) | Resto], 
                      [(Classe, NuovoConteggio) | Resto]
                     ) :-
  NuovoConteggio is Conteggio + 1.
aggiorna_map_conteggi(Classe, 
                      [(AltraClasse, Conteggio) | Resto], 
                      [(AltraClasse, Conteggio) | NuovoResto]
                     ) :-
  Classe \= AltraClasse,
  aggiorna_map_conteggi(Classe, Resto, NuovoResto).

/* Predicato per trovare la classe con il maggior numero di occorrenze */
classe_max_occorrenze([(Classe, Conteggio) | Resto], ClasseMaggioranza) :-
  classe_max_occorrenze(Resto, Classe, Conteggio, ClasseMaggioranza).

classe_max_occorrenze([], ClasseMaggioranza, _, ClasseMaggioranza).
classe_max_occorrenze([(Classe, Conteggio) | Resto], 
                      ClasseMax, ConteggioMax, ClasseMaggioranza) :-
  (Conteggio > ConteggioMax ->
   classe_max_occorrenze(Resto, Classe, Conteggio, ClasseMaggioranza)
   ;  
   classe_max_occorrenze(Resto, ClasseMax, ConteggioMax, ClasseMaggioranza)
  ).
	
% FUNZIONI AUSILIARIE	
	
/* Predicato per chiedere all'utente se vuole continuare */
richiedi_continuazione(Scelta) :-
  nl, write('Vuoi continuare? (s/n)'), nl,
  read(Scelta),
  (Scelta = 's' -> Scelta = 's' ;
   Scelta = 'n' -> Scelta = 'n' ;
   nl, write('Input incorretto. Inserisci "s" per continuare o "n" per uscire.'), 
   nl, richiedi_continuazione(Scelta)).

/* Predicato per stampare una linea orizzontale */
stampa_riga_orizzontale :- stampa_riga_orizzontale(65).

/* Specifica il numero di trattini nella linea orizzontale */
stampa_riga_orizzontale(N) :- 
  N > 0, 
  write('-'), 
  N1 is N - 1, 
  stampa_riga_orizzontale(N1).
stampa_riga_orizzontale(0) :- nl.
