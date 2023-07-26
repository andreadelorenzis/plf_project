-- Define the linear regression model as a tuple (slope, intercept)
type LinearRegressionModel = (Double, Double)

-- Function to calculate the mean of a list of numbers
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- Function to calculate the slope and intercept of the regression line
linearRegression :: [(Double, Double)] -> LinearRegressionModel
linearRegression dataPoints =
  let xs = map fst dataPoints
      ys = map snd dataPoints
      n = fromIntegral $ length dataPoints
      xySum = sum $ zipWith (*) xs ys
      xSum = sum xs
      ySum = sum ys
      xSquaredSum = sum $ map (^2) xs
      slope = (n * xySum - xSum * ySum) / (n * xSquaredSum - xSum ^ 2)
      intercept = (ySum - slope * xSum) / n
  in (slope, intercept)

-- Function to predict y values based on the linear regression model
predict :: LinearRegressionModel -> Double -> Double
predict (slope, intercept) x = slope * x + intercept

main :: IO ()
main = do
  let dataPoints = [(1, 2), (2, 3), (3, 5), (4, 4), (5, 6)]
      model = linearRegression dataPoints
      x = 6
      predictedY = predict model x

  putStrLn $ "Linear regression model: y = " ++ show (fst model) ++ "x + " ++ show (snd model)
  putStrLn $ "Predicted y for x = " ++ show x ++ ": " ++ show predictedY
