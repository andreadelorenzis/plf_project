{- Importazione delle librerie -}
import Text.Printf (printf)
import Data.List (sortOn)
import Data.Function (on)
import Text.Read (reads)
import Data.List (nub)
import Text.Read (readMaybe)

{- Definizione di un tipo per rappresentare punti in uno spazio 2D -}
data Point2D = Point2D { xCoord :: Double, yCoord :: Double } deriving Show

{- Definizione di un tipo per rappresentare i punti 2D etichettati con una classe (di tipo Int) -}
data PointLabeled = PointLabeled { x :: Double, y :: Double, label :: Char }

{- CALCOLO DELLA REGRESSIONE LINEARE -}

{- Calcola la media di una lista di Double -}
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

{- Calcola la varianza di una lista di Double -}
variance :: [Double] -> Double
variance xs = sum [(x - m) ** 2 | x <- xs] / fromIntegral (length xs)
  where
    m = mean xs

{- Calcola la covarianza di due liste di Double -}
covariance :: [Double] -> [Double] -> Double
covariance xs ys = sum [(x - mx) * (y - my) | (x, y) <- zip xs ys] / fromIntegral (length xs)
  where
    mx = mean xs
    my = mean ys

{- Calcola i coefficienti (pendenza e intercetta) della retta interpolatrice -}
linearRegression :: [Point2D] -> (Double, Double)
linearRegression points = (slope, intercept)
  where
    xs = map xCoord points
    ys = map yCoord points
    slope = covariance xs ys / variance xs
    intercept = mean ys - slope * mean xs

tupleToPoint2D :: [(Double, Double)] -> [Point2D]
tupleToPoint2D = map (\(x, y) -> Point2D x y)

{- Funzione per ottenere dall'utente una lista di punti separati da spazio -}
readPoints :: IO [Point2D]
readPoints = do
    putStrLn "\nInserisci i punti del dataset nel formato [(x1,y1), ..., (xn,yn)]:"
    input <- getLine
    let parsedList = read input :: [(Double, Double)]
    if length parsedList < 2
        then do
            putStrLn "Inserisci almeno due punti."
            readPoints
        else return $ tupleToPoint2D parsedList

{- Funzione che converte una lista di Double in una lista di Point2D -}
makePoints :: [Double] -> [Point2D]
makePoints [] = []
makePoints (x:y:rest) = Point2D x y : makePoints rest
makePoints _ = error "Numero incorretto di coordinate."

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
distance :: PointLabeled -> PointLabeled -> Double
distance p1 p2 = sqrt $ (x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2

{- Funziona che trova i k vicini per un punto dal dataset -}
kNearestNeighbors :: Int -> PointLabeled -> [PointLabeled] -> [PointLabeled]
kNearestNeighbors k testPoint dataset =
  take k $ sortOn (distance testPoint) dataset

{- Funzione che trova la classe di maggioranza tra i vicini -}

majorityClass :: [PointLabeled] -> Char
majorityClass neighbors =
  let allLabels = nub $ map label neighbors  -- Extract all unique labels from the neighbors
      counts = map (\label' -> (label', length (filter (\p -> label p == label') neighbors))) allLabels
      sortedCounts = reverse $ sortOn snd counts
  in fst $ head sortedCounts


tupleToPointLabeled :: [(Double, Double, Char)] -> [PointLabeled]
tupleToPointLabeled = map (\(x, y, l) -> PointLabeled x y l)

{- Funzione per ottenere dall'utente un dataset di punti etichettati con una classe -}
readLabeledDataset :: IO [PointLabeled]
readLabeledDataset = do
    putStrLn "\nInserisci i punti etichettati del dataset nel formato [(x1,y1,'<Char>'), ..., (xn,yn,'<Char>')]:"
    input <- getLine
    let parsedList = read input :: [(Double, Double, Char)]
    if length parsedList < 2
        then do
            putStrLn "\nErrore. Inserisci almeno due punti.\n"
            readLabeledDataset
        else return $ tupleToPointLabeled parsedList

{- Funzione per ottenere dall'utente un punto da testare sul KNN -}
readPoint :: IO PointLabeled
readPoint = do
  putStrLn "\nInserisci il punto da valutare nel formato 'x y':"
  input <- getLine
  let [x, y] = map read $ words input
  return $ PointLabeled x y 'Z'  -- The class label for the test point is not relevant, so we set it to Z.

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
    Nothing -> return ()  -- Return to the main menu

evaluateKNNLoop :: Int -> [PointLabeled] -> IO ()
evaluateKNNLoop k dataset = do
  testPoint <- readPoint
  let kNearest = kNearestNeighbors k testPoint dataset
      testClass = majorityClass kNearest
  putStrLn $ "\nClasse prevista per il punto: " ++ show testClass
  continue <- askContinue
  if continue
    then evaluateKNNLoop k dataset
    else return ()

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

{- Helper function to print a horizontal line -}
printHorizontalLine :: IO ()
printHorizontalLine = putStrLn "---------------------------------------------------------------"

{- MAIN -}

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
      points <- readPoints
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
      putStrLn "\nInserisci il valore di k (numero dei vicini):"
      k <- readLn :: IO Int
      evaluateKNNLoop k dataset
      mainMenu
      
    "3" -> putStrLn "\nChiusura del programma."
    _ -> do
      putStrLn "Scelta incorretta. Seleziona un'opzione valida."
      mainMenu

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