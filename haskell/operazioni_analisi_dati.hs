{- Programma Haskell per calcolare regressione lineare e k-nearest neighbors. -}

{- IMPORTAZIONE DELLE LIBRERIE -}

import Text.Printf (printf)          -- Per formattare le stringhe di output
import Data.List (sortOn, nub)       -- Per ordinare liste e rimuovere duplicati
import Data.Function (on)            -- Per ordinare in base a una funzione
import Text.Read (reads, readMaybe)  -- Per leggere input in modo sicuro
import Data.Char (isPrint)           -- Per sapere se un carattere è stampabile

{- Definizione di un tipo per rappresentare i punti. -}

data Punto = Punto { xCoord :: Double, yCoord :: Double } 
  deriving Show

{- Definizione di un tipo per rappresentare i punti etichettati. -}

data PuntoEtichettato = PuntoEtichettato { x :: Double, y :: Double, label :: String } 
  deriving Show

main :: IO ()
main = do
  putStrLn "\n"
  stampa_riga_orizzontale
  putStrLn "Progetto del corso di Programmazione Logica e Funzionale"
  putStrLn "Anno 2022/2023"
  putStrLn "Progetto realizzato da: Andrea De Lorenzis\n"
  menu_principale
  stampa_riga_orizzontale

{- Funzione che gestisce il menu delle operazioni. -}

menu_principale :: IO ()
menu_principale = do
  putStrLn "\nSelezionare l'operazione da svolgere:"
  putStrLn "1 - Regressione lineare"
  putStrLn "2 - K-Nearest neighbors"
  putStrLn "3 - Esci"

  choice <- getLine
  case choice of
    "1" -> do
      putStrLn ""
      putStrLn "-------- Regressione Lineare --------"
      punti <- leggi_dataset
      putStrLn "\nAdattamento della retta al dataset.."
      let (pendenza, intercetta) = calcola_coefficienti_retta punti
      let pendenza_formattata = printf "%.2f" pendenza
      let intercetta_formattata = printf "%.2f" intercetta
      putStrLn $ "\nRetta interpolatrice: y = " ++ pendenza_formattata 
                                                ++ "x + " 
                                                ++ intercetta_formattata
      valuta_valori_x (pendenza, intercetta)
      menu_principale
    "2" -> do
      putStrLn ""
      putStrLn "-------- K-Nearest Neighbors --------"
      dataset <- leggi_dataset_etichettato
      let num_punti = length dataset
      k <- leggi_valore_k num_punti
      valuta_punti k dataset
      menu_principale
    "3" -> putStrLn "\nChiusura del programma."
    _ -> do
      putStrLn "\nScelta incorretta. Seleziona un'opzione valida."
      menu_principale

{- CALCOLO REGRESSIONE LINEARE -}

{- Funzione che calcola i coefficienti (pendenza e intercetta) della retta sul dataset: 
   - il suo unico argomento è la lista di punti (dataset). -}
   
calcola_coefficienti_retta :: [Punto] -> (Double, Double)
calcola_coefficienti_retta punti = (pendenza, intercetta)
  where
    xs = map xCoord punti
    ys = map yCoord punti
    pendenza = covarianza xs ys / varianza xs
    intercetta = media ys - pendenza * media xs

{- Funzione che calcola la media di una lista di Double:
   - il suo unico argomento è la lista di Double. -}
   
media :: [Double] -> Double
media xs = sum xs / fromIntegral (length xs)

{- Funzione che calcola la varianza di una lista di Double:
   - il suo unico argomento è la lista di Double. -}

varianza :: [Double] -> Double
varianza xs = sum [(x - m) ** 2 | x <- xs] / fromIntegral (length xs)
  where
    m = media xs

{- Funzione che calcola la covarianza di due liste di Double
   - il primo argomento è la prima delle due liste;
   - il secondo argomento è la seconda delle due liste. -}

covarianza :: [Double] -> [Double] -> Double
covarianza xs ys = 
  sum [(x - mx) * (y - my) | (x, y) <- zip xs ys] / fromIntegral (length xs)
  where
    mx = media xs
    my = media ys

{- Funzione per acquisire una lista di punti nel corretto formato. -}

leggi_dataset :: IO [Punto]
leggi_dataset = do
  putStrLn "\nInserisci almeno due punti distinti nel formato [(x1,y1), ..., (xn,yn)]:"
  input <- getLine
  let lista_elaborata = readMaybe input :: Maybe [(Double, Double)]
  case lista_elaborata of
    Nothing -> do
      putStrLn $ "\nFormato non valido. Inserisci i punti nel formato " ++
                 "[(x1, y1), ..., (xn, yn)]."
      leggi_dataset
    Just punti -> 
      if length punti < 2 
      then do
        putStrLn "\nInput invalido. Inserisci almeno due punti."
        leggi_dataset
      else if length (nub punti) /= length punti
        then do
          putStrLn "\nInput invalido. Alcuni punti inseriti non sono distinti."
          leggi_dataset
        else return $ converti_tuple_in_punti punti

{- Funzione che converte una lista di coppie di Double in una lista di punti:
   il suo unico argomento è la lista di coppie. -}

converti_tuple_in_punti :: [(Double, Double)] -> [Punto]
converti_tuple_in_punti = map (\(x, y) -> Punto x y)

{- Funzione che cicla continuamente per valutare nuovi valori della coordinata x 
   sulla retta:
   - il suo unico argomento è una coppia di Double (pendenza e intercetta). -}

valuta_valori_x :: (Double, Double) -> IO ()
valuta_valori_x (pendenza, intercetta) = do
  valore <- leggi_valore
  case valore of
    Just x -> do
      let previsione = valuta_valore x (pendenza, intercetta)
      let previsione_formattata = printf "%.2f" previsione
      putStrLn $ "\nPer X = " ++ show x 
                              ++ " il valore previsto e' Y = " 
                              ++ previsione_formattata
      continua <- richiedi_continuazione
      if continua
        then valuta_valori_x (pendenza, intercetta)
        else return ()
    Nothing -> return ()  -- Ritorna al menu

{- Funzione che acquisisce un valore per la coordinata x. -}

leggi_valore :: IO (Maybe Double)
leggi_valore = do
  putStrLn "\nInserisci un valore per la coordinata x:"
  input <- getLine
  case readMaybe input of
    Just x -> return (Just x)
    Nothing -> do
      putStrLn "\nInput incorretto. Inserisci un numero valido."
      leggi_valore

{- Funzione che valuta un valore della coordinata x sulla retta:
   - il primo argomento è il valore della coordinata x;
   - il secondo argomento è una coppia di double (pendenza e intercetta). -}

valuta_valore :: Double -> (Double, Double) -> Double
valuta_valore x (pendenza, intercetta) = pendenza * x + intercetta

{- CALCOLO K-NEAREST NEIGHBORS -}

{- Funziona che trova i k vicini per un punto:
   - il primo argomento è il valore k dei vicini;
   - il secondo argomento è il punto di test;
   - il terzo argomento è una lista di punti etichettati (dataset). -}

trova_vicini :: Int -> PuntoEtichettato -> [PuntoEtichettato] -> [PuntoEtichettato]
trova_vicini k punto_test dataset =
  take k $ sortOn (distanza punto_test) dataset

{- Funzione che calcola la distanza euclidea tra due punti:
   - il primo argomento è il primo dei due punti;
   - il secondo argomento è il secondo dei due punti. -}

distanza :: PuntoEtichettato -> PuntoEtichettato -> Double
distanza p1 p2 = sqrt $ (x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2

{- Funzione che trova la classe di maggioranza tra i vicini:
   - l'unico argomento è l'insieme dei k punti vicini. -}

trova_classe_maggioranza :: [PuntoEtichettato] -> String
trova_classe_maggioranza vicini = fst $ head conteggi_ordinati
  where
    etichette = nub $ map label vicini
    conteggi = map (\label' -> 
                     (label', length (filter (\p -> label p == label') vicini))
                   ) etichette
    conteggi_ordinati = reverse $ sortOn snd conteggi

{- Funzione per acquisire un dataset di punti etichettati. -}

leggi_dataset_etichettato :: IO [PuntoEtichettato]
leggi_dataset_etichettato = do
  putStrLn $ "\nInserisci almeno due punti distinti ed etichettati nel formato " ++
             "[(x1,y1,\"<classe>\"), ..., (xn,yn,\"<classe>\")]:"
  input <- getLine
  case reads input of
    [(triple, "")] ->
      if all punto_etichettato_valido triple && length triple >= 2 && punti_distinti triple
        then return $ converti_triple_in_punti triple
        else do
          putStrLn $ "\nInput invalido. " ++ messaggio_errore triple
          leggi_dataset_etichettato
    _ -> do
      putStrLn $ "\nInput invalido. Assicurati che l'input sia nel formato " ++ 
                 "[(x1,y1,\"<classe>\"), ..., (xn,yn,\"<classe>\")]."
      leggi_dataset_etichettato
  where
    messaggio_errore ps
      | length ps < 2 = "Inserisci almeno due punti."
      | not (all punto_etichettato_valido ps) = 
          "Formato non valido. Controlla che ogni " ++ 
          "punto sia nel formato (x, y, \"<classe>\")."
      | not (punti_distinti ps) = "Tutti i punti devono essere distinti."
      | otherwise = ""

{- Funzione che verifica se tutti i punti in un dataset etichettato sono distinti: 
   - il suo unico argomento è la lista di punti etichettati. -}

punti_distinti :: [(Double, Double, String)] -> Bool
punti_distinti ps = length solo_punti == length (nub solo_punti)
  where
    solo_punti = [(x, y) | (x, y, _) <- ps]

{- Funzione che converte una lista di triple in una lista di punti etichettati:
   - l'unico argomento è la lista di triple di valori. -}

converti_triple_in_punti :: [(Double, Double, String)] -> [PuntoEtichettato]
converti_triple_in_punti = map (\(x, y, l) -> PuntoEtichettato x y l)

{- Funzione per assicurarsi che il carattere per il punto etichettato sia stampabile:
   - l'unico argomento è la tripla di valori che rappresentano il punto etichettato.
   Per esempio, non deve essere un carattere di controllo come '\n'. -}

punto_etichettato_valido :: (Double, Double, String) -> Bool
punto_etichettato_valido (_, _, c) = all isPrint c

{- Funzione per acquisire il valore di k:
   - l'unico argomento è il numero di punti nel dataset.
   Il numero dei vicini deve essere infatti minore uguale del numero di punti. -}

leggi_valore_k :: Int -> IO Int
leggi_valore_k num_punti = do
  putStrLn $ "\nInserisci il valore di k (numero dei vicini, 1-" ++ show num_punti 
                                                                 ++ "):"
  input <- getLine
  case readMaybe input of
    Just k
      | k > 0 && k <= num_punti -> return k
      | otherwise -> do
          putStrLn $ "\nIl valore di k deve essere un "  ++ 
                     "intero positivo compreso tra 1 e " ++ 
                                          show num_punti ++ "."
          leggi_valore_k num_punti
    Nothing -> do
      putStrLn "\nInput non valido. Inserisci un numero intero valido."
      leggi_valore_k num_punti

{- Funzione che cicla continuamente per valutare nuovi punti sul KNN:
   - il primo argomento è il valore k dei vicini;
   - il secondo argomento è la lista di punti etichettati. -}

valuta_punti :: Int -> [PuntoEtichettato] -> IO ()
valuta_punti k dataset = do
  punto_test <- leggi_punto
  let vicini = trova_vicini k punto_test dataset
      classe_prevista = trova_classe_maggioranza vicini
  stampa_vicini vicini
  putStrLn $ "\nClasse prevista per il punto: " ++ show classe_prevista
  continua <- richiedi_continuazione
  if continua
    then valuta_punti k dataset
    else return ()

{- Funzione per acquisire un punto di test. -}

leggi_punto :: IO PuntoEtichettato
leggi_punto = do
  putStrLn "\nInserisci il punto da valutare nel formato '(x, y)':"
  input <- getLine
  case readMaybe input of
    Just (x, y) -> return $ PuntoEtichettato x y "Z"  -- Etichetta non significativa
    Nothing -> do
      putStrLn "\nInput incorretto. Inserisci un punto nel formato '(x, y)'."
      leggi_punto

{- Funzione per stampare i vicini del punto di test:
   - l'unico argomento è la lista di tutti i vicini. -}

stampa_vicini :: [PuntoEtichettato] -> IO ()
stampa_vicini vicini = do
  putStrLn "\nI vicini del punto sono:"
  mapM_ stampa_vicino vicini
    where
      stampa_vicino (PuntoEtichettato x y lbl) =
        putStrLn $ "Punto: (" ++ show x ++ ", " ++ show y ++ ", " ++ lbl ++ ")"

{- FUNZIONI AUSILIARIE -}

{- Funzione ausiliaria per chiedere all'utente se vuole continuare -}

richiedi_continuazione :: IO Bool
richiedi_continuazione = do
  putStrLn "\nVuoi continuare? (s/n)"
  input <- getLine
  case input of
    "s" -> return True
    "n" -> return False
    _ -> do
      putStrLn $ "\nInput invalido. Inserisci 's' per continuare o 'n' per " ++ 
               "ritornare al menu principale"
      richiedi_continuazione

{- Funzione ausiliaria per stampare una riga orizzontale -}

stampa_riga_orizzontale :: IO ()
stampa_riga_orizzontale = putStrLn $ "--------------------------------" ++ 
                                     "--------------------------------"
