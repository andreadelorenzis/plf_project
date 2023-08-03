{- IMPORTAZIONE DELLE LIBRERIE -}

{- Formattazione e output -}
import Text.Printf (printf)  -- Utilizzato per formattare le stringhe di output
{- Gestione delle liste -}
import Data.List (sortOn, nub)  -- Utilizzato per ordinare liste e rimuovere duplicati
import Data.Function (on)  -- Utilizzato per personalizzare l'ordinamento in base a una funzione
{- Conversione e lettura -}
import Text.Read (reads, readMaybe)  -- Utilizzato per leggere input in modo sicuro e per effettuare parsing
{- Controllo dei caratteri -}
import Data.Char (isPrint)           

{- MAIN -}

{- Punto iniziale del programma -}
main :: IO ()
main = do
  putStrLn "\n"
  printHorizontalLine
  putStrLn "Progetto del corso di Programmazione Logica e Funzionale"
  putStrLn "Anno 2022/2023"
  putStrLn "Progetto realizzato da: Andrea De Lorenzis\n"
  mainMenu
  printHorizontalLine

{- Funzione Main contenente il menu di selezione dell'operazione da svolgere -}
mainMenu :: IO ()
mainMenu = do
  putStrLn "\nSelezionare l'operazione da svolgere:"
  putStrLn "1 - Linear Regression"
  putStrLn "2 - K-Nearest Neighbors (KNN)"
  putStrLn "3 - Esci"

  choice <- getLine
  case choice of
    "1" -> do
      putStrLn ""
      putStrLn "-------- Linear Regression --------"
      points <- readDataset
      putStrLn "\nAdattamento della retta al dataset.."
      let (slope, intercept) = linearRegression points
      let formattedSlope = printf "%.2f" slope
      let formattedIntercept = printf "%.2f" intercept
      putStrLn $ "\nRetta interpolatrice: y = " ++ formattedSlope ++ "x + " ++ formattedIntercept
      evaluateXValuesLoop (slope, intercept)
      mainMenu

    "2" -> do
      putStrLn ""
      putStrLn "-------- K-Nearest Neighbors --------"
      dataset <- readLabeledDataset
      let numPoints = length dataset
      k <- readKValue numPoints
      evaluatePointsLoop k dataset
      mainMenu
      
    "3" -> putStrLn "\nChiusura del programma."
    _ -> do
      putStrLn "\nScelta incorretta. Seleziona un'opzione valida."
      mainMenu

{- DEFINIZIONE DI STRUTTURE DATI -}

{- Definizione di un tipo per rappresentare punti in uno spazio 2D -}
data Point2D = Point2D { xCoord :: Double, yCoord :: Double } deriving Show

{- Definizione di un tipo per rappresentare i punti 2D etichettati con una classe (di tipo Int) -}
data LabeledPoint2D = LabeledPoint2D { x :: Double, y :: Double, label :: String } deriving Show

{- CALCOLO DELLA REGRESSIONE LINEARE -}

{- Funzione che calcola la media di una lista di Double -}
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

{- Funzione che calcola la varianza di una lista di Double -}
variance :: [Double] -> Double
variance xs = sum [(x - m) ** 2 | x <- xs] / fromIntegral (length xs)
  where
    m = mean xs

{- Funzione che calcola la covarianza di due liste di Double -}
covariance :: [Double] -> [Double] -> Double
covariance xs ys = sum [(x - mx) * (y - my) | (x, y) <- zip xs ys] / fromIntegral (length xs)
  where
    mx = mean xs
    my = mean ys

{- Funzione che calcola i coefficienti (pendenza e intercetta) della retta interpolatrice -}
linearRegression :: [Point2D] -> (Double, Double)
linearRegression points = (slope, intercept)
  where
    xs = map xCoord points
    ys = map yCoord points
    slope = covariance xs ys / variance xs
    intercept = mean ys - slope * mean xs

{- Funzione che converte una coppia di Double in una struttura Point2D -}
tupleToPoint2D :: [(Double, Double)] -> [Point2D]
tupleToPoint2D = map (\(x, y) -> Point2D x y)

{- Funzione per ottenere dall'utente una lista di punti separati da spazio -}
readDataset :: IO [Point2D]
readDataset = do
    putStrLn "\nInserisci i punti del dataset nel formato [(x1,y1), ..., (xn,yn)]:"
    input <- getLine
    let parsedList = readMaybe input :: Maybe [(Double, Double)]
    case parsedList of
        Nothing -> do
            putStrLn "\nFormato non valido. Inserisci i punti nel formato [(x1, y1), ..., (xn, yn)]."
            readDataset
        Just points -> if length points < 2
                       then do
                           putStrLn "\nInput invalido. Inserisci almeno due punti."
                           readDataset
                       else return $ tupleToPoint2D points

{- Funzione che cicla continuamente per valutare nuovi valori di x -}
evaluateXValuesLoop :: (Double, Double) -> IO ()
evaluateXValuesLoop (slope, intercept) = do
  xValue <- readXValue
  case xValue of
    Just x -> do
      let prediction = evaluateXValue x (slope, intercept)
      let formattedPrediction = printf "%.2f" prediction
      putStrLn $ "\nPer X = " ++ show x ++ " il valore previsto e' Y = " ++ formattedPrediction
      continue <- askContinue
      if continue
        then evaluateXValuesLoop (slope, intercept)
        else return ()
    Nothing -> return ()  -- Ritorna al menu

{- Funzione che ottiene dall'utente un punto da valutare sulla retta ottenuta -}
readXValue :: IO (Maybe Double)
readXValue = do
  putStrLn "\nInserisci un valore per la coordinata x:"
  input <- getLine
  case readMaybe input of
    Just x -> return (Just x)
    Nothing -> do
      putStrLn "\nInput incorretto. Inserisci un numero valido."
      readXValue

{- Funzione che valuta la regressione lineare per il punto -}
evaluateXValue :: Double -> (Double, Double) -> Double
evaluateXValue x (slope, intercept) = slope * x + intercept

{- CALCOLO DEL KNN -}

{- Funzione che calcola la distanza euclidea tra due punti -}
distance :: LabeledPoint2D -> LabeledPoint2D -> Double
distance p1 p2 = sqrt $ (x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2

{- Funziona che trova i k vicini per un punto dal dataset -}
kNearestNeighbors :: Int -> LabeledPoint2D -> [LabeledPoint2D] -> [LabeledPoint2D]
kNearestNeighbors k testPoint dataset =
  take k $ sortOn (distance testPoint) dataset

{- Funzione che trova la classe di maggioranza tra i vicini -}
majorityClass :: [LabeledPoint2D] -> String
majorityClass neighbors =
  let allLabels = nub $ map label neighbors  -- Extract all unique labels from the neighbors
      counts = map (\label' -> (label', length (filter (\p -> label p == label') neighbors))) allLabels
      sortedCounts = reverse $ sortOn snd counts
  in fst $ head sortedCounts

{- Funzione per ottenere dall'utente un dataset di punti etichettati con una classe -}
readLabeledDataset :: IO [LabeledPoint2D]
readLabeledDataset = do
    putStrLn "\nInserisci i punti etichettati del dataset nel formato [(x1,y1,'<class>'), ..., (xn,yn,'<class>')]:"
    input <- getLine
    case reads input of
        [(points, "")] ->
            if all isValidLabeledPoint points && length points >= 2
            then return $ tupleToLabeledPoint2D points
            else do
                putStrLn $ "\nErrore. " ++ errorMsg points
                readLabeledDataset
        _ -> do
            putStrLn "\nErrore. Assicurati che l'input sia nel formato [(x1,y1,'<class>'), ..., (xn,yn,'<class>')]."
            readLabeledDataset
    where
        errorMsg ps
            | length ps < 2 = "Inserisci almeno due punti."
            | not (all isValidLabeledPoint ps) = "Formato non valido. Controlla che ogni punto sia nel formato (x, y, '<class>') dove '<class>' è un carattere o una combinazione di caratteri."
            | otherwise = ""

{- Converte una tripla (Double, Double, C) in una struttura LabeledPoint2D -}
tupleToLabeledPoint2D :: [(Double, Double, String)] -> [LabeledPoint2D]
tupleToLabeledPoint2D = map (\(x, y, l) -> LabeledPoint2D x y l)

{- Funzione per assicurarsi che il carattere sia stampabile -}
isValidLabeledPoint :: (Double, Double, String) -> Bool
isValidLabeledPoint (_, _, c) = all isPrint c -- Ensures the character is printable (i.e., not a control character)

{- Funzione per ottenere dall'utente il valore di k -}
readKValue :: Int -> IO Int
readKValue numPoints = do
    putStrLn $ "\nInserisci il valore di k (numero dei vicini, 1-" ++ show numPoints ++ "):"
    input <- getLine
    case readMaybe input of
        Just k
            | k > 0 && k <= numPoints -> return k
            | otherwise -> do
                putStrLn $ "\nIl valore di k deve essere un intero positivo compreso tra 1 e " ++ show numPoints ++ "."
                readKValue numPoints
        Nothing -> do
            putStrLn "\nInput non valido. Inserisci un numero intero valido."
            readKValue numPoints

{- Funzione che cicla continuamente per valutare nuovi punti -}
evaluatePointsLoop :: Int -> [LabeledPoint2D] -> IO ()
evaluatePointsLoop k dataset = do
  testPoint <- readPoint
  let kNearest = kNearestNeighbors k testPoint dataset
      testClass = majorityClass kNearest
  printNeighbors kNearest
  putStrLn $ "\nClasse prevista per il punto: " ++ show testClass
  continue <- askContinue
  if continue
    then evaluatePointsLoop k dataset
    else return ()

{- Funzione per ottenere dall'utente un punto da testare sul KNN -}
readPoint :: IO LabeledPoint2D
readPoint = do
  putStrLn "\nInserisci il punto da valutare nel formato '(x, y)':"
  input <- getLine
  case readMaybe input of
    Just (x, y) -> return $ LabeledPoint2D x y "Z"  -- Qui l'etichetta della classe non è significativa
    Nothing -> do
      putStrLn "\nInput incorretto. Inserisci un punto nel formato '(x, y)'."
      readPoint

{- Funzione per stampare i vicini -}
printNeighbors :: [LabeledPoint2D] -> IO ()
printNeighbors neighbors = do
    putStrLn "\nI vicini del punto sono:"
    mapM_ printNeighbor neighbors
  where
    printNeighbor (LabeledPoint2D x y lbl) =
      putStrLn $ "Punto: (" ++ show x ++ ", " ++ show y ++ ", " ++ lbl ++ ")"

{- FUNZIONI AUSILIARIE -}

{- Funzione ausiliaria per chiedere all'utente se vuole continuare -}
askContinue :: IO Bool
askContinue = do
  putStrLn "\nVuoi continuare? (s/n)"
  input <- getLine
  case input of
    "s" -> return True
    "n" -> return False
    _ -> do
      putStrLn "\nInput invalido. Inserisci 's' per continuare o 'n' per ritornare al menu principale"
      askContinue

{- Funzione ausiliaria per stampare una riga orizzontale -}
printHorizontalLine :: IO ()
printHorizontalLine = putStrLn "---------------------------------------------------------------"