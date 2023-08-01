% ESECUZIONE MAIN

/* Punto iniziale del programma */
main :-
  nl, nl, 
  print_horizontal_line,
  write('Progetto del corso di Programmazione Logica e Funzionale'), nl,
  write('Anno 2022/2023'), nl,
  write('Progetto realizzato da: Andrea De Lorenzis'), nl, nl,
  main_menu,
  print_horizontal_line.
  
/* Menu di selezione dell'operazione da svolgere */
main_menu :-
  nl, write('Selezionare l\'operazione da svolgere: '), nl,
  write('1 - Linear Regression'), nl,
  write('2 - K-Nearest Neighbors (KNN)'), nl,
  write('3 - Esci'), nl,
  
  read(Choice),
  (Choice = 1 -> doLinearRegression, main_menu ;
   Choice = 2 -> doKNN, main_menu ;
   Choice = 3 -> nl, write('Chiusura del programma.'), nl ;
   write('Scelta incorretta. Per favore, seleziona una delle opzioni valide.'), nl, 
   main_menu). 
   
/* Funzione per eseguire la regressione lineare */
doLinearRegression :-
  nl, write('-------- Linear Regression --------'), nl,
  readPoints(Points),
  nl, write('Adattamento della retta al dataset..'), nl,
  linearRegression(Points, Slope, Intercept), 
  nl,
  format('Retta interpolatrice: y = ~2fx + ~2f~n', [Slope, Intercept]),
  evaluateXValues(Slope, Intercept).
  
/* Funzione per eseguire il KNN */
doKNN :- 
    nl, write('-------- K-Nearest Neighbors --------'), nl,
    readLabeledDataset(Dataset),
	length(Dataset, NumPoints),
    repeat,
    nl, write('Inserisci il valore per k: '), nl,
    read(K),
    (   number(K) -> 
        K > 0,
		(K =< NumPoints ->
            !
		;
		    nl, write('Input invalido. Inserisci un intero positivo '),
			write('minore del (o uguale al) numero totale di punti nel dataset'), nl,
			format('(Numero di punti: ~d)', [NumPoints]), nl,
            fail
		)
    ;
        nl, write('Input invalido. Inserisci un intero positivo.'), nl,
        fail
    ),
    evaluatePoints(Dataset, K).

 
% CALCOLO REGRESSIONE LINEARE

/* Calcola la media di una lista di numeri */
mean([], 0).
mean([X | Xs], Mean) :-
    mean(Xs, RestMean),
    length(Xs, N),
    Mean is (X + N * RestMean) / (N + 1).
  
/* Calcola la varianza di una lista di numeri */  
variance([], 0).
variance([X | Xs], Var) :-
    mean([X | Xs], Mean),
    variance(Xs, RestVar),
    Var is RestVar + (X - Mean) * (X - Mean).
	
/* Calcola la covarianza di due liste di numeri */
covariance([], [], 0).
covariance([X | Xs], [Y | Ys], Cov) :-
  mean([X | Xs], MeanX),
  mean([Y | Ys], MeanY),
  covariance(Xs, Ys, RestCov),
  Cov is RestCov + (X - MeanX) * (Y - MeanY).
  
/* Calcola i coefficienti (pendenza e intercetta) della retta interpolatrice */
linearRegression(Points, Slope, Intercept) :-
  extractXCoords(Points, Xs),
  extractYCoords(Points, Ys),
  variance(Xs, VarX),
  covariance(Xs, Ys, CovXY),
  Slope is CovXY / VarX,
  mean(Ys, MeanY),
  mean(Xs, MeanX),
  Intercept is MeanY - Slope * MeanX.
 
/* Estra le coordinate X da una lista di punti */
extractXCoords([], []).
extractXCoords([(X, _) | Points], [X | Xs]) :-
  extractXCoords(Points, Xs).
  
/* Estrae le coordinate Y da una lista di punti */
extractYCoords([], []).
extractYCoords([(_, Y) | Points], [Y | Ys]) :-
  extractYCoords(Points, Ys).
  
/* Predicato per leggere una lista di tuple da tastiera */
readPoints(List) :-
    repeat,
    nl, write('Inserisci i punti del dataset nel formato: [(x1,y1), ..., (xn,yn)]:'), nl,
    read(List),
    (validPointList(List) ->
        !  % Success: List is a valid list of points
    ;
        nl, write('Input invalido. La lista deve contenere almeno due punti nel formato [(x,y), ...].'), nl,
        fail % Failure: Invalid list format, retry the input
    ).

/* Predicato per validare una lista di punti 2D */
validPointList(List) :-
    is_list(List),
    length(List, NumPoints),
    NumPoints >= 2,
    forall(member(Point, List), is_point(Point)).
  
/* Predicato per controllare che un punto sia nel formato (x,y) */
is_point(Point) :- 
    Point = (X, Y),
    number(X),
    number(Y).
  
/* Funzione che ottiene dall'utente una coordinata X da valutare sulla retta ottenuta */
readXValue(Value) :-
    repeat,
    nl, write('Inserisci un valore per la coordinata x: '), nl,
    read(Value),
    (   number(Value) ->
        true  % Success: Value is a number, the cut (!) will prevent further backtracking
    ;
        nl, write('Il valore inserito non e'' un numero.'), nl,
        fail % Failure: Value is not a number, retry the input
    ).

/* Predicato che cicla continuamente per valutare i punti e chiede 
   all'utente se vuole continuare */
evaluateXValues(Slope, Intercept) :-
    readXValue(Value),
    evaluateXValue(Value, Slope, Intercept, Y),
    nl, format('Per X = ~2f il valore previsto e\' Y = ~2f', [Value, Y]), nl,
    askContinue(Continue),
    (Continue = 's' -> evaluateXValues(Slope, Intercept) ; true).

/* Funzione che valuta la regressione lineare per il punto */
evaluateXValue(X, Slope, Intercept, Y) :-
  Y is Slope * X + Intercept.

% CALCOLO K-NEAREST NEIGHBORS

/* Funzione che calcola la distanza euclidea tra due punti */
distance((X1, Y1, _), (X2, Y2), Dist) :-
  Dist is sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)).
  
/* Funzione che trova i k vicini per un punto */
kNearestNeighbors(K, TestPoint, Dataset, Neighbors) :-
  calculateDistances(TestPoint, Dataset, Distances),
  sortDistances(Distances, SortedDistances),
  take(K, SortedDistances, NeighborsPairs),
  convertToNeighbors(NeighborsPairs, Neighbors).
  
/* Rimuove il valore della distanza dal formato (Dist-(X, Y, Class)) 
   per ottenere il formato (X, Y, Class) */
convertToNeighbors([], []).
convertToNeighbors([_-Neighbor | RestPairs], [Neighbor | RestNeighbors]) :-
    convertToNeighbors(RestPairs, RestNeighbors).

/* Predicato per calolcare le distanze tra TestPoint e ogni altro punto del dataset */
calculateDistances(_, [], []).
calculateDistances(TestPoint, [Point | Rest], [Dist-Point | Distances]) :-
  distance(Point, TestPoint, Dist),
  calculateDistances(TestPoint, Rest, Distances).
  
/* Ordina le distanze in ordine crescente */
sortDistances(Distances, SortedDistances) :-
  keysort(Distances, SortedDistances).

/* Prende i primi K elementi di una lista */
take(0, _, []).
take(K, [X | Xs], [X | Rest]) :-
  K > 0,
  K1 is K - 1,
  take(K1, Xs, Rest).
  
/* Funzione che trova la classe di maggioranza tra i vicini */
majorityClass(Neighbors, MajorityClass) :-
    countClassOccurrences(Neighbors, CountMap),
    maxClassOccurrences(CountMap, MajorityClass).

/* Predicato ausiliario per conteggiare le occorrenze di ogni 
  classe nei vicini */
countClassOccurrences(Neighbors, CountMap) :-
    countClassOccurrences(Neighbors, [], CountMap).

countClassOccurrences([], CountMap, CountMap).
countClassOccurrences([(X, Y, Class) | Neighbors], PartialCountMap, CountMap) :-
    updateCountMap(Class, PartialCountMap, NewCountMap),
    countClassOccurrences(Neighbors, NewCountMap, CountMap).

updateCountMap(Class, [], [(Class, 1)]).
updateCountMap(Class, [(Class, Count) | Rest], [(Class, NewCount) | Rest]) :-
    NewCount is Count + 1.
updateCountMap(Class, [(OtherClass, Count) | Rest], [(OtherClass, Count) | NewRest]) :-
    Class \= OtherClass,
    updateCountMap(Class, Rest, NewRest).

/* Predicato ausiliario per trovare la classe con il maggior numero di 
  occorrenze */
maxClassOccurrences([(Class, Count) | Rest], MajorityClass) :-
    maxClassOccurrences(Rest, Class, Count, MajorityClass).

maxClassOccurrences([], MajorityClass, _, MajorityClass).
maxClassOccurrences([(Class, Count) | Rest], MaxClass, MaxCount, MajorityClass) :-
    ( Count > MaxCount ->
        maxClassOccurrences(Rest, Class, Count, MajorityClass)
    ;   maxClassOccurrences(Rest, MaxClass, MaxCount, MajorityClass)
    ).

/* Funzione per ottenere dall'utente un dataset di punti etichettati con 
   una classe */
readLabeledDataset(List) :-
    repeat,
    nl, write('Inserisci i punti del dataset nel formato: [(x1,y1,C), ..., (xn,yn,C)].'), nl,
    read(List),
    (validLabeledDataset(List) ->
        !  % Success: List is a valid labeled dataset
    ;
        nl, write('Invalid input. The list must contain at least two labeled points in the format [(x,y,C), ...].'), nl,
        fail % Failure: Invalid list format, retry the input
    ).
	
/* Predicato di validazione per un dataset con etichette */
validLabeledDataset(List) :-
    is_list(List),
    length(List, NumPoints),
    NumPoints >= 2,
    forall(member(LabeledPoint, List), is_labeled_point(LabeledPoint)).

/* Predicato per controllare se un elemento è un punto etichettato valido nel 
   formato (X, Y, C) */
is_labeled_point(LabeledPoint) :- 
    LabeledPoint = (X, Y, C),
    number(X),
    number(Y),
    atom(C).

/* Funzione per leggere un singolo punto da valutare con KNN */
readLabeledPoint(Point) :-
    repeat,
    nl, write('Inserisci il valore del punto da testare nel formato (x,y): '), nl,
    read(Point),
    validPoint(Point),
    !.

/* Regola di validazione per un punto etichettato */
validPoint(Point) :-
    is_point(Point),
    !.
validPoint(_) :-
    nl, write('Punto non valido. Assicurati di inserire un punto nel formato (x,y).'), nl,
    fail.

/* Funzione per continuamente inserire punti da valutare con KNN */
evaluatePoints(_, 0) :- !.
evaluatePoints(Dataset, K) :-
  readLabeledPoint(TestPoint),
  kNearestNeighbors(K, TestPoint, Dataset, Neighbors),
  nl, format('I ~d vicini del punto sono: ', [K]), nl, 
  printNeighbors(Neighbors), nl, 
  majorityClass(Neighbors, Class),
  format('La classe prevista per il punto e\': ~w~n', [Class]), 
  askContinue(Continue),
  (Continue = 's' -> evaluatePoints(Dataset, K) ; true).

/* Funzione per stampare i vicini nel formato (x, y, C) */
printNeighbors([]).
printNeighbors([(X, Y, C) | Rest]) :-
    format('Punto: (~2f, ~2f, ~w)~n', [X, Y, C]),
    printNeighbors(Rest).
	
% FUNZIONI AUSILIARIE	
	
/* Predicato ausiliare per chiedere all'utente se vuole continuare */
askContinue(Continue) :-
    nl, write('Vuoi continuare? (s/n)'), nl,
    read(Choice),
    (Choice = 's' -> Continue = 's' ;
     Choice = 'n' -> Continue = 'n' ;
     nl, write('Input incorretto. Inserisci "s" per continuare o "n" per uscire.'), nl, 
     askContinue(Continue)).

/* Predicato per stampare una linea orizzontale */
print_horizontal_line :- print_horizontal_line(65).

/* Specifica il numero di trattini nella linea orizzontale */
print_horizontal_line(N) :- N > 0, write('-'), N1 is N - 1, print_horizontal_line(N1).
print_horizontal_line(0) :- nl.
