% ESECUZIONE MAIN

/* Punto iniziale del programma */
main :-
  nl, nl, 
  write('Progetto del corso di Programmazione Logica e Funzionale'), nl,
  write('Anno 2022/2023'), nl,
  write('Progetto realizzato da: Andrea De Lorenzis'), nl, nl, nl,
  main_menu.
  
/* Menu di selezione dell'operazione da svolgere */
main_menu :-
  write('Selezionare l\'operazione da svolgere: '), nl,
  write('1 - Linear Regression'), nl,
  write('2 - K-Nearest Neighbors (KNN)'), nl,
  write('3 - Quit'), nl, nl,
  
  read(Choice),
  (Choice = 1 -> doLinearRegression, main_menu ;
   Choice = 2 -> doKNN, main_menu ;
   Choice = 3 -> write('Chiusura del programma.'), nl ;
   write('Scelta incorretta. Per favore, seleziona una delle opzioni valide.'), nl, 
   main_menu). 
   
/* Funzione per eseguire la regressione lineare */
doLinearRegression :-
  readDataset(Points),
  nl, nl, write(Points), nl, nl.
  
  /*linearRegression(Points, Slope, Intercept), 
  nl,
  format('Best-fitting line: y = ~2f*x + ~2f~n', [Slope, Intercept]), nl,
  evaluateXValues(Slope, Intercept).*/
 
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
  
/* Predicato per leggere una lista di tuple da file */
readDataset(List) :-
  open('dataset.txt', read, Stream),
  read_line(Stream, Tuple),
  close(Stream).

read_digit(Stream, Digit) :-
    read(Stream, Digit), integer(Digit), Digit >= 1, Digit =< 9.

read_line(end_of_file, _) :- !.
  
read_line(Stream, Tuple) :-
  read_digit(Stream, X),
  read_digit(Stream, Y),
  write(X), write(' '), write(Y),
  Tuple is (X, Y).

process_stream(end_of_file, _) :- !.
 
process_stream(Char, Stream) :-
        write(Char),
        get_char(Stream, Char2),
        process_stream(Char2, Stream).

/*
  nl, write('Inserisci i punti del dataset nel formato: [(x1,y1), ..., (xn,yn)].'), nl, nl,
  read(List),
  length(List, NumPoints),
  (NumPoints < 2 -> write('Sono necessari almeno due punti per la regressione lineare'), readDataset([]), nl ; true).
  */
  
/* Funzione che ottiene dall'utente una coordinata X da valutare sulla retta ottenuta */
readXValue(Value) :-
  write('Inserisci un valore per la coordinata x (o \'q\' per tornare al menu): '), nl, nl,
  read(Value).
  
/* Predicato che cicla continuamente per valutare i punti fin a quando 
   l'utente non inserisce 'q' */
evaluateXValues(Slope, Intercept) :-
  readXValue(Value),
  (Value = 'q' -> true ;
   evaluateXValue(Value, Slope, Intercept, Y),
   nl, format('Il valore di Y previsto e\': ~2f', [Y]), nl,
   evaluateXValues(Slope, Intercept)).

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
  nl, write('Inserisci i punti del dataset nel formato: [(x1,y1,C), ..., (xn,yn,C)].'), nl, nl,
  read(List), nl,
  length(List, NumPoints),
  (NumPoints < 2 -> write('Sono necessari almeno due punti per la regressione lineare'), readLabeledDataset([]), nl ; true).
  
/* Funzione per leggere un singolo punto da valutare con KNN */
readLabeledPoint(Point) :-
  write('Inserisci il valore del punto da testare nel formato (x,y): '), nl, nl,
  read(Point).
  
/* Funzione per eseguire il KNN */
doKNN :- 
  readLabeledDataset(Dataset),
  nl, write('Inserisci il valore per k: '), nl, nl,
  read(K), nl, nl,
  evaluatePoints(Dataset, K).

/* Funzione per continuamente inserire punti da valutare con KNN */
evaluatePoints(Dataset, K) :-
  readLabeledPoint(TestPoint),
  kNearestNeighbors(K, TestPoint, Dataset, Neighbors),
  displayNeighbors(Neighbors),
  majorityClass(Neighbors, Class),
  intToChar(ClassInt, Class),
  nl, nl, format('La classe prevista per il punto è: ~w~n', [Class]), 
  nl, nl, write('Vuoi continuare? (si/no)'), nl, nl,
  read(Choice),
  (Choice = 'si' -> evaluatePoints(Dataset, K) ; true).
  
displayNeighbors([]).
displayNeighbors([Dist-Point | Rest]) :-
  Point = (X, Y, ClassInt),
  intToChar(ClassInt, Class),
  format('Distanza: ~2f, Punto: (~2f, ~2f, ~w)~n', [Dist, X, Y, Class]),
  displayNeighbors(Rest).
  
% Predicate to convert integer class values to characters
intToChar(0, 'A').
intToChar(1, 'B').
intToChar(2, 'C').
intToChar(3, 'D').
