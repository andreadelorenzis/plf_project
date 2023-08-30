/* Programma Prolog per calcolare regressione lineare e k-nearest neighbors. */

main :-
  nl, nl, 
  stampa_riga_orizzontale,
  write('Progetto del corso di Programmazione Logica e Funzionale'), nl,
  write('Anno 2022/2023'), nl,
  write('Progetto realizzato da: Andrea De Lorenzis'), nl, nl,
  menu_principale,
  stampa_riga_orizzontale.
  
/* Predicato per gestire il menu delle operazioni. */

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

/* Predicato per eseguire la regressione lineare. */

esegui_regressione_lineare :-
  nl, write('-------- Regressione Lineare --------'), nl,
  leggi_dataset(Punti),
  nl, write('Adattamento della retta al dataset..'), nl,
  calcola_coefficienti_retta(Punti, Pendenza, Intercetta), 
  nl,
  (Pendenza = 'undefined' ->  % Caso di retta verticale
   write('Retta interpolatrice: y = NaNx + NaN'), nl
   ;
   format('Retta interpolatrice: y = ~2fx + ~2f~n', [Pendenza, Intercetta])
  ),
  valuta_valori_x(Pendenza, Intercetta).

/* Predicato per leggere una lista di punti da tastiera. */

leggi_dataset(Lista) :-
  nl, write('Inserisci almeno due punti distinti nel formato '),
  write('[(x1,y1), ..., (xn,yn)]: '),
  nl, read(Input),
  ( 
    % Controllo che la lista sia nel formato corretto
    (\+ is_list(Input) 
     ; 
     \+ forall(member(Punto, Input), punto_valido(Punto)))
   ->
     nl, write('Input invalido. Assicurati di inserire una lista nel formato '),
     write('[(x1,y1), ..., (xn,yn)].'), nl,
     leggi_dataset(Lista)
   ;
     % Controllo che ci siano almeno due punti
     length(Input, NumPunti),
     NumPunti < 2
   ->
     nl, write('Input invalido. Inserisci almeno due punti.'), nl,
     leggi_dataset(Lista)
   ;
     % Controllo che tutti i punti siano distinti
     \+ punti_distinti(Input)
   ->
     nl, write('Input invalido. Tutti i punti devono essere distinti.'), nl,
     leggi_dataset(Lista)
   ;
     Lista = Input
  ).
  
  
/* Predicato per controllare che un punto sia nel formato (x,y):
   - il suo unico argomento è il punto. */

punto_valido(Punto) :- 
  Punto = (X, Y),
  number(X),
  number(Y).
  
/* Predicato che verifica se tutti i punti sono distinti:
   - il suo unico argomento è la lista di punti. */

punti_distinti([]).
punti_distinti([Testa|Coda]) :- 
  \+ member(Testa, Coda),
  punti_distinti(Coda).

/* Predicato che calcola i coefficienti (pendenza e intercetta) della retta:
   - il suo unico argomento è la lista di punti che forma il dataset. */

calcola_coefficienti_retta(Punti, Pendenza, Intercetta) :-
  estrai_coordinate_x(Punti, Xs),
  estrai_coordinate_y(Punti, Ys),
  varianza(Xs, VarX),
  (VarX =:= 0 -> % Se VarX è zero, gestisco il caso di retta verticale
   Pendenza = 'undefined',
   Intercetta = 'undefined'
   ; 
   covarianza(Xs, Ys, CovXY),
   Pendenza is CovXY / VarX,
   media(Ys, MediaY),
   media(Xs, MediaX),
   Intercetta is MediaY - Pendenza * MediaX
  ).
 
/* Predicato che estrae le coordinate X da una lista di punti:
   - il suo unico argomento è la lista. */

estrai_coordinate_x([], []).
estrai_coordinate_x([(X, _) | Punti], [X | Xs]) :-
  estrai_coordinate_x(Punti, Xs).
  
/* Predicato che estrae le coordinate Y da una lista di punti:
   - il suo unico argomento è la lista. */

estrai_coordinate_y([], []).
estrai_coordinate_y([(_, Y) | Punti], [Y | Ys]) :-
  estrai_coordinate_y(Punti, Ys).

/* Predicato che calcola la varianza di una lista di numeri:
   - il suo unico argomento è la lista. */  

varianza([], 0).
varianza([X | Xs], Var) :-
  media([X | Xs], Media),
  varianza(Xs, Resto),
  Var is Resto + (X - Media) * (X - Media).
	
/* Predicato che calcola la covarianza di due liste di numeri:
   - il primo argomento è la prima delle due liste; 
   - il secondo argomento è la seconda delle due liste. */

covarianza([], [], 0).
covarianza([X | Xs], [Y | Ys], Cov) :-
  media([X | Xs], MediaX),
  media([Y | Ys], MediaY),
  covarianza(Xs, Ys, Resto),
  Cov is Resto + (X - MediaX) * (Y - MediaY).

/* Predicato che calcola la media di una lista di numeri:
   - il suo unico argomento è la lista.*/

media([], 0).
media([X | Xs], Media) :-
  media(Xs, Resto),
  length(Xs, N),
  Media is (X + N * Resto) / (N + 1).

/* Predicato che cicla continuamente per valutare nuovi valori della coordinata x:
   - il primo valore è la pendenza della retta;
   - il secondo valore è l'intercetta della retta. */

valuta_valori_x(Pendenza, Intercetta) :-
  leggi_valore(X),
  valuta_valore(X, Pendenza, Intercetta, Y),
  (Pendenza = 'undefined' -> % Caso di retta verticale
   nl, format('Per X = ~2f il valore previsto e\' Y = NaN', [X]), nl
   ;
   nl, format('Per X = ~2f il valore previsto e\' Y = ~2f', [X, Y]), nl
  ),
  richiedi_continuazione(Scelta),
  (Scelta = 's' -> valuta_valori_x(Pendenza, Intercetta) ; true).

/* Predicato che acquisisce un valore per la coordinata X. */

leggi_valore(X) :-
  nl, write('Inserisci un valore per la coordinata x: '), nl,
  read(Val),
  (number(Val) ->
   X = Val
   ;
   nl, write('Il valore inserito non e\' un numero.'), nl,
   leggi_valore(X) 
  ).

/* Predicato che valuta un valore della coordinata x sulla retta:
  - il primo argomento è il valore della coordinata x;
  - il secondo argomento è la pendenza della retta;
  - il terzo argomento è l'intercetta della retta. */

valuta_valore(X, Pendenza, Intercetta, Y) :-
  (Pendenza = 'undefined' ->  % Caso di retta verticale
   Y is 0  % valore non significativo
   ;
   Y is Pendenza * X + Intercetta
  ).

% CALCOLO K-NEAREST NEIGHBORS

/* Predicato per eseguire il KNN. */

esegui_knn :- 
  nl, write('-------- K-Nearest Neighbors --------'), nl,
  leggi_dataset_etichettato(Dataset),
  length(Dataset, NumPunti),
  leggi_valore_k(NumPunti, K),
  valuta_punti(Dataset, K).
  
/* Predicato che acquisisce il valore k dei vicini:
   - il suo unico argomento è il numero dei punti nel dataset. */

leggi_valore_k(NumPunti, K) :-
  nl, format('Inserisci il valore di k (numero dei vicini, 1-~d): ', [NumPunti]), nl,
  read(Input),
  (number(Input), Input > 0, Input =< NumPunti ->
   K = Input
   ;
   nl, write('Input invalido. Il valore di k deve essere un intero positivo '),
   format('compreso tra 1 e ~d.', [NumPunti]), nl,
   leggi_valore_k(NumPunti, K)  % Ricorsione: chiede di nuovo all'utente il valore di k
  ).

/* Predicato per acquisire un dataset di punti etichettati. */

leggi_dataset_etichettato(Lista) :-
  nl, write('Inserisci i punti del dataset nel formato '),
  write('[(x1,y1,<classe>), ..., (xn,yn,<classe>)]: '), nl,
  read(Input),
  ( 
   % Controllo che la lista sia nel formato corretto
    (\+ is_list(Input) 
     ; 
     \+ forall(member(Punto, Input), punto_etichettato_valido(Punto)))
   ->
     nl, write('Input invalido. Assicurati di inserire una lista nel formato '),
     write('[(x1,y1,<classe>), ..., (xn,yn,<classe>)].'), nl,
     leggi_dataset_etichettato(Lista)
   ;
     % Controllo che ci siano almeno due punti
     length(Input, NumPunti),
     NumPunti < 2
   ->
     nl, write('Input invalido. Inserisci almeno due punti.'), nl,
     leggi_dataset_etichettato(Lista)
   ;
     % Controllo che tutti i punti siano distinti
     \+ punti_etichettati_distinti(Input)
   ->
     nl, write('Input invalido. Tutti i punti devono essere distinti.'), nl,
     leggi_dataset_etichettato(Lista)
   ;
     Lista = Input
  ).

/* Predicato per validare un punto etichettato:
   - il suo unico argomento è il punto in questione. */

punto_etichettato_valido(Punto) :- 
  Punto = (X, Y, C),
  number(X),
  number(Y),
  atom(C).

/* Predicato che verifica se tutti i punti etichettati sono distinti:
   - il suo unico argomento è la lista di punti etichettati. */

punti_etichettati_distinti([]).
punti_etichettati_distinti([(X, Y, _)|Coda]) :- 
  \+ member((X, Y, _), Coda),
  punti_etichettati_distinti(Coda).


/* Predicato che cicla continuamente per valutare nuovi punti sul KNN:
   - il primo argomento è la lista di punti etichettati;
   - il secondo argomento è il numero k dei vicini. */

valuta_punti(_, 0) :- !.
valuta_punti(Dataset, K) :-
  leggi_punto_etichettato(Punto),
  trova_vicini(K, Punto, Dataset, Vicini),
  nl, write('I vicini del punto sono: '), nl, 
  stampa_vicini(Vicini), nl, 
  trova_classe_maggioranza(Vicini, Classe),
  format('La classe prevista per il punto e\': ~w~n', [Classe]), 
  richiedi_continuazione(Scelta),
  (Scelta = 's' -> valuta_punti(Dataset, K) ; true).

/* Predicato che acquisisce un punto di test. */

leggi_punto_etichettato(Punto) :-
  nl, write('Inserisci il valore del punto da testare nel formato (x,y): '), nl,
  read(Input),
  (punto_valido(Input) ->
   Punto = Input
   ;
   nl, write('Punto non valido. Assicurati di inserire un punto '),
   write('nel formato (x,y).'), nl,
   leggi_punto_etichettato(Punto)
  ).

/* Predicato che trova i k vicini per un punto:
   - il primo argomento è il numero k dei vicini;
   - il secondo argomento è il punto di test;
   - il terzo argomento è il dataset di punti etichettati. */

trova_vicini(K, Punto, Dataset, Vicini) :-
  calcola_distanze(Punto, Dataset, Distanze),
  ordina_distanze(Distanze, DistanzeOrdinate),
  prendi(K, DistanzeOrdinate, CoppieVicini),
  converti_coppie_in_vicini(CoppieVicini, Vicini).

/* Predicato che calcola la distanza tra il punto di test e tutti gli altri punti: 
   - il primo argomento è il punto di test;
   - il secondo argomento è il dataset di punti.
   Crea un'associazione distanza-punto, annotando ogni punto del dataset con la 
   sua distanza dal punto di test. */

calcola_distanze(_, [], []).
calcola_distanze(PuntoTest, [Punto | Resto], [Dist-Punto | Distanze]) :-
  distanza(Punto, PuntoTest, Dist),
  calcola_distanze(PuntoTest, Resto, Distanze).
  
/* Predicato che calcola la distanza euclidea tra due punti: 
   - il primo argomento è il primo dei due punti;
   - il secondo argomento è il secondo dei due punti. */

distanza((X1, Y1, _), (X2, Y2), Dist) :-
  Dist is sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)).

/* Predicato che ordina le distanze in ordine crescente:
   - il suo unico argomento è la lista di associazioni Dist-Punto */

ordina_distanze(Distanze, DistanzeOrdinate) :-
  keysort(Distanze, DistanzeOrdinate).

/* Predicato che prende i primi K elementi di una lista:
   - il suo unico argomento è la lista summenzionata. */

prendi(0, _, []).
prendi(K, [X | Xs], [X | Resto]) :-
  K > 0,
  K1 is K - 1,
  prendi(K1, Xs, Resto).
  
/* Predicato che elimina l'informazione sulla distanza dal formato Dist-Punto 
   per ottenere solamente il punto etichettato:
   - il suo unico argomento è la lista di associazioni Dist-Punto */

converti_coppie_in_vicini([], []).
converti_coppie_in_vicini([_-Vicino | CoppieRestanti], [Vicino | ViciniRestanti]) :-
  converti_coppie_in_vicini(CoppieRestanti, ViciniRestanti).
  
/* Predicato che stampa tutti i vicini: 
   - il suo unico argomento è la lista di vicini. */

stampa_vicini([]).
stampa_vicini([(X, Y, C) | Resto]) :-
  format('Punto: (~2f, ~2f, ~w)~n', [X, Y, C]),
  stampa_vicini(Resto).
  
/* Predicato che trova la classe di maggioranza tra i vicini:
   - il suo unico argomento è la lista di vicini. */

trova_classe_maggioranza(Vicini, ClasseMaggioranza) :-
  conta_occorrenze_classe(Vicini, MapConteggi),
  classe_max_occorrenze(MapConteggi, ClasseMaggioranza).

/* Predicato che conteggia le occorrenze di ogni classe nei vicini: 
   - il primo argomento è la lista di vicini;
   - il secondo argomento è la mappa di conteggi corrente (inizialmente vuota).
   La mappa di conteggi è una lista di coppie, dove ogni coppia è composta da
   una classe e il suo corrispondente conteggio. */

conta_occorrenze_classe(Vicini, MapConteggi) :-
  conta_occorrenze_classe(Vicini, [], MapConteggi).
  
conta_occorrenze_classe([], MapConteggi, MapConteggi).
conta_occorrenze_classe([(_, _, Classe) | Vicini], MapConteggiParziale, MapConteggi) :-
  aggiorna_map_conteggi(Classe, MapConteggiParziale, NuovaMapConteggi),
  conta_occorrenze_classe(Vicini, NuovaMapConteggi, MapConteggi).

/* Predicato che aggiorna una mappa di conteggi:
   - il primo argomento è la classe di cui aggiornare il conteggio;
   - il secondo argomento è la mappa di conteggi corrente. */

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

/* Predicato che trova la classe con il maggior numero di occorrenze:
   - il suo unico argomento è la mappa dei conteggi. */
   
classe_max_occorrenze([(Classe, Conteggio) | Resto], ClasseMaggioranza) :-
  classe_max_occorrenze_aux(Resto, Classe, Conteggio, ClasseMaggioranza).

/* Predicato ausiliario usato dal predicato principale classe_max_occorrenze:
   - il primo argomento è la lista di elementi rimanenti nella mappa conteggi;
   - il secondo argomento è la classe con conteggio massimo trovata finora;
   - il terzo argomento è il conteggio massimo trovato finora per suddetta classe. 
   Se esiste un'altra classe nella mappa con conteggio superiore a ConteggioMax, 
   quella diventa la nuova classe di maggioranza. */

classe_max_occorrenze_aux([], ClasseMaggioranza, _, ClasseMaggioranza).
classe_max_occorrenze_aux([(Classe, Conteggio) | Resto], 
                      ClasseMax, ConteggioMax, ClasseMaggioranza) :-
  (Conteggio > ConteggioMax ->
   classe_max_occorrenze_aux(Resto, Classe, Conteggio, ClasseMaggioranza)
   ;  
   classe_max_occorrenze_aux(Resto, ClasseMax, ConteggioMax, ClasseMaggioranza)
  ).
   
% FUNZIONI AUSILIARIE	
	
/* Predicato per chiedere all'utente se vuole continuare */

richiedi_continuazione(Scelta) :-
  nl, write('Vuoi continuare? (s/n)'), nl,
  read(SceltaInput),
  (atom(SceltaInput), SceltaInput = 's' -> Scelta = 's' ;
   atom(SceltaInput), SceltaInput = 'n' -> Scelta = 'n' ;
   nl, write('Input incorretto. Inserisci "s" per continuare o "n" per uscire.'), 
   nl, richiedi_continuazione(Scelta)).

/* Predicato che stampa un certo numero di trattini su una linea orizzontale:
   - il suo unico argomento è il numero di trattini.*/

stampa_riga_orizzontale :- stampa_riga_orizzontale(65).

stampa_riga_orizzontale(N) :- 
  N > 0, 
  write('-'), 
  N1 is N - 1, 
  stampa_riga_orizzontale(N1).
stampa_riga_orizzontale(0) :- nl.
