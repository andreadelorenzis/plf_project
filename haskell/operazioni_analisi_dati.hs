{- Programma Haskell per calcolare regressione lineare e k-nearest neighbors -}

{- IMPORTAZIONE DELLE LIBRERIE -}

{- Formattazione e output -}
import Text.Printf (printf)          -- Utilizzata per formattare le stringhe di output
{- Gestione delle liste -}
import Data.List (sortOn, nub)       -- Utilizzata per ordinare liste e rimuovere duplicati
import Data.Function (on)            -- Utilizzata per personalizzare l'ordinamento in base a una funzione
{- Conversione e lettura -}
import Text.Read (reads, readMaybe)  -- Utilizzata per leggere input in modo sicuro e per effettuare parsing
{- Controllo dei caratteri -}
import Data.Char (isPrint)           -- Utilizzata per determinare se un carattere è stampabile o meno

{- ESECUZIONE MAIN -}

{- Punto iniziale del programma -}
main :: IO ()
main = do
  putStrLn "\n"
  stampa_riga_orizzontale
  putStrLn "Progetto del corso di Programmazione Logica e Funzionale"
  putStrLn "Anno 2022/2023"
  putStrLn "Progetto realizzato da: Andrea De Lorenzis\n"
  menu_principale
  stampa_riga_orizzontale

{- Funzione Main contenente il menu di selezione dell'operazione da svolgere -}
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
      let (pendenza, intercetta) = regressione_lineare punti
      let pendenza_formattata = printf "%.2f" pendenza
      let intercetta_formattata = printf "%.2f" intercetta
      putStrLn $ "\nRetta interpolatrice: y = " ++ pendenza_formattata ++ "x + " ++ intercetta_formattata
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

{- DEFINIZIONE DI STRUTTURE DATI -}

{- Definizione di un tipo per rappresentare punti in uno spazio 2D -}
data Punto = Punto { xCoord :: Double, yCoord :: Double } deriving Show

{- Definizione di un tipo per rappresentare i punti 2D etichettati con una classe -}
data PuntoEtichettato = PuntoEtichettato { x :: Double, y :: Double, label :: String } deriving Show

{- CALCOLO REGRESSIONE LINEARE -}

{- Funzione che calcola la media di una lista di Double -}
media :: [Double] -> Double
media xs = sum xs / fromIntegral (length xs)

{- Funzione che calcola la varianza di una lista di Double -}
varianza :: [Double] -> Double
varianza xs = sum [(x - m) ** 2 | x <- xs] / fromIntegral (length xs)
  where
    m = media xs

{- Funzione che calcola la covarianza di due liste di Double -}
covarianza :: [Double] -> [Double] -> Double
covarianza xs ys = sum [(x - mx) * (y - my) | (x, y) <- zip xs ys] / fromIntegral (length xs)
  where
    mx = media xs
    my = media ys

{- Funzione che calcola i coefficienti (pendenza e intercetta) della retta interpolatrice -}
regressione_lineare :: [Punto] -> (Double, Double)
regressione_lineare punti = (pendenza, intercetta)
  where
    xs = map xCoord punti
    ys = map yCoord punti
    pendenza = covarianza xs ys / varianza xs
    intercetta = media ys - pendenza * media xs

{- Funzione che converte una coppia di Double in una struttura Punto -}
converti_tupla_in_punto :: [(Double, Double)] -> [Punto]
converti_tupla_in_punto = map (\(x, y) -> Punto x y)

{- Funzione per ottenere dall'utente una lista di punti separati da spazio -}
leggi_dataset :: IO [Punto]
leggi_dataset = do
  putStrLn "\nInserisci i punti del dataset nel formato [(x1,y1), ..., (xn,yn)]:"
  input <- getLine
  let lista_elaborata = readMaybe input :: Maybe [(Double, Double)]
  case lista_elaborata of
    Nothing -> do
      putStrLn "\nFormato non valido. Inserisci i punti nel formato [(x1, y1), ..., (xn, yn)]."
      leggi_dataset
    Just punti -> if length punti < 2
                    then do
                      putStrLn "\nInput invalido. Inserisci almeno due punti."
                      leggi_dataset
                    else return $ converti_tupla_in_punto punti

{- Funzione che cicla continuamente per valutare nuovi valori di x -}
valuta_valori_x :: (Double, Double) -> IO ()
valuta_valori_x (pendenza, intercetta) = do
  valore <- leggi_valore
  case valore of
    Just x -> do
      let previsione = valuta_valore x (pendenza, intercetta)
      let previsione_formattata = printf "%.2f" previsione
      putStrLn $ "\nPer X = " ++ show x ++ " il valore previsto e' Y = " ++ previsione_formattata
      continua <- richiedi_continuazione
      if continua
        then valuta_valori_x (pendenza, intercetta)
        else return ()
    Nothing -> return ()  -- Ritorna al menu

{- Funzione che ottiene dall'utente un punto da valutare sulla retta ottenuta -}
leggi_valore :: IO (Maybe Double)
leggi_valore = do
  putStrLn "\nInserisci un valore per la coordinata x:"
  input <- getLine
  case readMaybe input of
    Just x -> return (Just x)
    Nothing -> do
      putStrLn "\nInput incorretto. Inserisci un numero valido."
      leggi_valore

{- Funzione che valuta la regressione lineare per il punto -}
valuta_valore :: Double -> (Double, Double) -> Double
valuta_valore x (pendenza, intercetta) = pendenza * x + intercetta

{- CALCOLO K-NEAREST NEIGHBORS -}

{- Funzione che calcola la distanza euclidea tra due punti -}
distanza :: PuntoEtichettato -> PuntoEtichettato -> Double
distanza p1 p2 = sqrt $ (x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2

{- Funziona che trova i k vicini per un punto dal dataset -}
k_nearest_neighbors :: Int -> PuntoEtichettato -> [PuntoEtichettato] -> [PuntoEtichettato]
k_nearest_neighbors k punto_test dataset =
  take k $ sortOn (distanza punto_test) dataset

{- Funzione che trova la classe di maggioranza tra i vicini -}
trova_classe_maggioranza :: [PuntoEtichettato] -> String
trova_classe_maggioranza vicini = fst $ head conteggi_ordinati
  where
    etichette = nub $ map label vicini
    conteggi = map (\label' -> (label', length (filter (\p -> label p == label') vicini))) etichette
    conteggi_ordinati = reverse $ sortOn snd conteggi

{- Funzione per ottenere dall'utente un dataset di punti etichettati con una classe -}
leggi_dataset_etichettato :: IO [PuntoEtichettato]
leggi_dataset_etichettato = do
  putStrLn "\nInserisci i punti etichettati del dataset nel formato [(x1,y1,'<class>'), ..., (xn,yn,'<class>')]:"
  input <- getLine
  case reads input of
    [(punti, "")] ->
      if all punto_etichettato_valido punti && length punti >= 2
        then return $ converti_tripla_in_punto_etichettato punti
        else do
          putStrLn $ "\nErrore. " ++ messaggio_errore punti
          leggi_dataset_etichettato
    _ -> do
      putStrLn "\nErrore. Assicurati che l'input sia nel formato [(x1,y1,'<class>'), ..., (xn,yn,'<class>')]."
      leggi_dataset_etichettato
  where
    messaggio_errore ps
      | length ps < 2 = "Inserisci almeno due punti."
      | not (all punto_etichettato_valido ps) = "Formato non valido. Controlla che ogni punto sia nel formato (x, y, '<class>')."
      | otherwise = ""

{- Converte una tripla (Double, Double, C) in una struttura PuntoEtichettato -}
converti_tripla_in_punto_etichettato :: [(Double, Double, String)] -> [PuntoEtichettato]
converti_tripla_in_punto_etichettato = map (\(x, y, l) -> PuntoEtichettato x y l)

{- Funzione per assicurarsi che il carattere sia stampabile (es. non un carattere di controllo -}
punto_etichettato_valido :: (Double, Double, String) -> Bool
punto_etichettato_valido (_, _, c) = all isPrint c

{- Funzione per ottenere dall'utente il valore di k -}
leggi_valore_k :: Int -> IO Int
leggi_valore_k num_punti = do
  putStrLn $ "\nInserisci il valore di k (numero dei vicini, 1-" ++ show num_punti ++ "):"
  input <- getLine
  case readMaybe input of
    Just k
      | k > 0 && k <= num_punti -> return k
      | otherwise -> do
          putStrLn $ "\nIl valore di k deve essere un intero positivo compreso tra 1 e " ++ show num_punti ++ "."
          leggi_valore_k num_punti
    Nothing -> do
      putStrLn "\nInput non valido. Inserisci un numero intero valido."
      leggi_valore_k num_punti

{- Funzione che cicla continuamente per valutare nuovi punti -}
valuta_punti :: Int -> [PuntoEtichettato] -> IO ()
valuta_punti k dataset = do
  punto_test <- leggi_punto
  let vicini = k_nearest_neighbors k punto_test dataset
      classe_prevista = trova_classe_maggioranza vicini
  stampa_vicini vicini
  putStrLn $ "\nClasse prevista per il punto: " ++ show classe_prevista
  continua <- richiedi_continuazione
  if continua
    then valuta_punti k dataset
    else return ()

{- Funzione per ottenere dall'utente un punto da testare sul KNN -}
leggi_punto :: IO PuntoEtichettato
leggi_punto = do
  putStrLn "\nInserisci il punto da valutare nel formato '(x, y)':"
  input <- getLine
  case readMaybe input of
    Just (x, y) -> return $ PuntoEtichettato x y "Z"  -- Qui l'etichetta della classe non è significativa
    Nothing -> do
      putStrLn "\nInput incorretto. Inserisci un punto nel formato '(x, y)'."
      leggi_punto

{- Funzione per stampare i vicini -}
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
      putStrLn "\nInput invalido. Inserisci 's' per continuare o 'n' per ritornare al menu principale"
      richiedi_continuazione

{- Funzione ausiliaria per stampare una riga orizzontale -}
stampa_riga_orizzontale :: IO ()
stampa_riga_orizzontale = putStrLn "---------------------------------------------------------------"
