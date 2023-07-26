module Main where

-- Define a data type to represent points in 2D space
data Point2D = Point2D { xCoord :: Double, yCoord :: Double } deriving Show

-- Calculate the mean of a list of Doubles
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- Calculate the variance of a list of Doubles
variance :: [Double] -> Double
variance xs = sum [(x - m) ** 2 | x <- xs] / fromIntegral (length xs)
  where
    m = mean xs

-- Calculate the covariance of two lists of Doubles
covariance :: [Double] -> [Double] -> Double
covariance xs ys = sum [(x - mx) * (y - my) | (x, y) <- zip xs ys] / fromIntegral (length xs)
  where
    mx = mean xs
    my = mean ys

-- Calculate the coefficients (slope and intercept) of the best-fitting line
linearRegression :: [Point2D] -> (Double, Double)
linearRegression points = (slope, intercept)
  where
    xs = map xCoord points
    ys = map yCoord points
    slope = covariance xs ys / variance xs
    intercept = mean ys - slope * mean xs

-- Read a list of space-separated points from user input
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

-- Convert a list of Doubles to a list of Point2D
makePoints :: [Double] -> [Point2D]
makePoints [] = []
makePoints (x:y:rest) = Point2D x y : makePoints rest
makePoints _ = error "Invalid number of coordinates."

-- Read a single point to evaluate from user input
readPointToEvaluate :: IO (Maybe Point2D)
readPointToEvaluate = do
  putStrLn "Enter the point to evaluate in the format 'x y' (or 'q' to quit):"
  input <- getLine
  if input == "q"
    then return Nothing
    else do
      let [x, y] = map read $ words input
      return $ Just $ Point2D x y

-- Evaluate the linear regression at the given point
evaluatePoint :: Point2D -> (Double, Double) -> Double
evaluatePoint (Point2D x _) (slope, intercept) = slope * x + intercept

main :: IO ()
main = do
  points <- readPoints
  let (slope, intercept) = linearRegression points

  putStrLn $ "Best-fitting line: y = " ++ show slope ++ "x + " ++ show intercept

  -- Interactive loop for evaluation
  evalLoop slope intercept

evalLoop :: Double -> Double -> IO ()
evalLoop slope intercept = do
  mPoint <- readPointToEvaluate
  case mPoint of
    Just pointToEvaluate -> do
      let result = evaluatePoint pointToEvaluate (slope, intercept)
      putStrLn $ "Evaluated point (" ++ show (xCoord pointToEvaluate) ++ "): " ++ show result
      evalLoop slope intercept
    Nothing -> putStrLn "Exiting the program."
