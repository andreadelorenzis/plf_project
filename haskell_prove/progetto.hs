{- Importazione delle librerie -}
import Text.Printf (printf)
import Data.List (sortOn)
import Data.Function (on)

{- Definizione di un tipo per rappresentare punti in uno spazio 2D -}
data Point2D = Point2D { xCoord :: Double, yCoord :: Double } deriving Show

{- Definizione di un tipo per rappresentare i punti 2D etichettati con una classe (di tipo Int) -}
data PointLabeled = PointLabeled { x :: Double, y :: Double, label :: Int }

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

{- Funzione per ottenere dall'utente una lista di punti separati da spazio -}
readPoints :: IO [Point2D]
readPoints = do
  putStrLn "Enter the dataset in the format 'x1 y1 x2 y2 ... xn yn':"
  input <- getLine
  let coordinates = map read $ words input
  let points = makePoints coordinates
  if length points < 2
    then do
      putStrLn "At least two points are required for linear regression."
      readPoints
    else return points

{- Funzione che converte una lista di Double in una lista di Point2D -}
makePoints :: [Double] -> [Point2D]
makePoints [] = []
makePoints (x:y:rest) = Point2D x y : makePoints rest
makePoints _ = error "Invalid number of coordinates."

{- Funzione che ottiene dall'utente un punto da valutare sulla retta ottenuta -}
readPointToEvaluate :: IO (Maybe Point2D)
readPointToEvaluate = do
  putStrLn "Enter the point to evaluate in the format 'x y' (or 'q' to quit):"
  input <- getLine
  if input == "q"
    then return Nothing
    else do
      let [x, y] = map read $ words input
      return $ Just $ Point2D x y

{- Funzione che valuta la regressione lineare per il punto -}
evaluatePoint :: Point2D -> (Double, Double) -> Double
evaluatePoint (Point2D x _) (slope, intercept) = slope * x + intercept

{- CALCOLO DEL KNN -}

{- Funzione che calcola la distanza euclidea tra due punti -}
distance :: PointLabeled -> PointLabeled -> Double
distance p1 p2 = sqrt $ (x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2

{- Funziona che trova i k vicini per un punto dal dataset -}
kNearestNeighbors :: Int -> PointLabeled -> [PointLabeled] -> [PointLabeled]
kNearestNeighbors k testPoint dataset =
  take k $ sortOn (distance testPoint) dataset

{- Funzione che trova la classe di maggioranza tra i vicini -}
majorityClass :: [PointLabeled] -> Int
majorityClass neighbors =
  let counts = map (\label' -> (label', length (filter (\p -> label p == label') neighbors))) [0, 1, 2]
      sortedCounts = reverse $ sortOn snd counts
  in fst $ head sortedCounts

{- Funzione per ottenere dall'utente un dataset di punti etichettati con una classe -}
readLabeledDataset :: Int -> [PointLabeled] -> IO [PointLabeled]
readLabeledDataset 0 acc = return $ reverse acc
readLabeledDataset n acc = do
  input <- getLine
  let [x, y, label] = map read $ words input
  readLabeledDataset (n-1) (PointLabeled x y (round label) : acc)  -- Convert label to Int using `round`

{- Funzione per ottenere dall'utente un punto da testare sul KNN -}
readPoint :: IO PointLabeled
readPoint = do
  input <- getLine
  let [x, y] = map read $ words input
  return $ PointLabeled x y 0  -- The class label for the test point is not relevant, so we set it to 0.

{- MAIN -}

{- Funzione Main contenente il menu di selezione dell'operazione da svolgere -}
mainMenu :: IO ()
mainMenu = do
  putStrLn "Choose the operation to compute:"
  putStrLn "1 - Linear Regression"
  putStrLn "2 - k-Nearest Neighbors (KNN)"
  putStrLn "3 - Quit"

  choice <- getLine
  case choice of
    "1" -> do
      points <- readPoints
      let (slope, intercept) = linearRegression points
      let formattedSlope = printf "%.2f" slope
      let formattedIntercept = printf "%.2f" intercept
      putStrLn $ "Best-fitting line: y = " ++ formattedSlope ++ "x + " ++ formattedIntercept
      mainMenu
    "2" -> do
      putStrLn "Enter the number of data points in the dataset:"
      n <- readLn :: IO Int

      putStrLn "Enter the dataset (each line should have format: x y classLabel):"
      dataset <- readLabeledDataset n []

      putStrLn "Enter the value of k:"
      k <- readLn :: IO Int

      putStrLn "Enter the test point (format: x y):"
      testPoint <- readPoint

      let kNearest = kNearestNeighbors k testPoint dataset
          testClass = majorityClass kNearest

      putStrLn $ "Predicted class for the test point: " ++ show testClass
      mainMenu
    "3" -> putStrLn "Exiting the program."
    _ -> do
      putStrLn "Invalid choice. Please select a valid option."
      mainMenu

{- Punto iniziale del programma -}
main :: IO ()
main = do
  mainMenu
