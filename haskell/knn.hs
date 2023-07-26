import Data.List (sortOn)
import Data.Function (on)

data Point = Point { x :: Double, y :: Double, label :: Int } deriving (Show)

distance :: Point -> Point -> Double
distance p1 p2 = sqrt $ (x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2

kNearestNeighbors :: Int -> Point -> [Point] -> [Point]
kNearestNeighbors k testPoint dataset =
  take k $ sortOn (distance testPoint) dataset

majorityClass :: [Point] -> Int
majorityClass neighbors =
  let counts = map (\label' -> (label', length (filter (\p -> label p == label') neighbors))) [0, 1, 2]
      sortedCounts = reverse $ sortOn snd counts
  in fst $ head sortedCounts

main :: IO ()
main = do
  putStrLn "Enter the number of data points in the dataset:"
  n <- readLn :: IO Int

  putStrLn "Enter the dataset (each line should have format: x y classLabel):"
  dataset <- readDataset n []

  putStrLn "Enter the value of k:"
  k <- readLn :: IO Int

  putStrLn "Enter the test point (format: x y):"
  testPoint <- readPoint

  let kNearest = kNearestNeighbors k testPoint dataset
      testClass = majorityClass kNearest

  putStrLn $ "Predicted class for the test point: " ++ show testClass

readDataset :: Int -> [Point] -> IO [Point]
readDataset 0 acc = return $ reverse acc
readDataset n acc = do
  input <- getLine
  let [x, y, label] = map read $ words input
  readDataset (n-1) (Point x y (round label) : acc)  -- Convert label to Int using `round`

readPoint :: IO Point
readPoint = do
  input <- getLine
  let [x, y] = map read $ words input
  return $ Point x y 0  -- The class label for the test point is not relevant, so we set it to 0.
