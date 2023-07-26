import Control.Monad (replicateM)

type Cell = Bool
type Board = [[Cell]]

elementAt :: Board -> Int -> Int -> Cell
elementAt board row col = board !! row !! col

countAdjacentOrganisms :: Board -> Int -> Int -> Int
countAdjacentOrganisms board row col = length $ filter id neighbors
  where
    boardRows = length board
    boardCols = length (head board)

    getWrappedRow r = (r + boardRows) `mod` boardRows
    getWrappedCol c = (c + boardCols) `mod` boardCols

    neighbors = [ elementAt board (getWrappedRow i) (getWrappedCol j)
                | i <- [row - 1 .. row + 1]
                , j <- [col - 1 .. col + 1]
                , i /= row || j /= col
                ]

evolveCell :: Cell -> Int -> Cell
evolveCell True  n | n < 2 || n > 3 = False
                   | otherwise      = True
evolveCell False n | n == 3    = True
                   | otherwise = False

evolveBoard :: Board -> Board
evolveBoard board = [[evolveCell (elementAt board i j) (countAdjacentOrganisms board i j) | j <- [0..boardCols-1]] | i <- [0..boardRows-1]]
  where
    boardRows = length board
    boardCols = length (head board)

printBoard :: Board -> String
printBoard = unlines . map (map cellToChar)
  where
    cellToChar True  = 'O'
    cellToChar False = '.'

playGame :: Board -> IO ()
playGame board = loop board
  where
    boardRows = length board
    boardCols = length (head board)

    loop :: Board -> IO ()
    loop currentBoard = do
      putStrLn "Press Enter to see the next generation or 'q' to quit: "
      input <- getLine
      case input of
        "" -> do
          putStrLn "Next Generation:"
          putStrLn $ printBoard currentBoard
          let nextGeneration = evolveBoard currentBoard
          loop nextGeneration
        "q" -> putStrLn "Exiting the game."
        _   -> do
          putStrLn "Invalid input. Please try again."
          loop currentBoard

main :: IO ()
main = do
  putStrLn "Enter the number of rows for the grid: "
  numRows <- readLn :: IO Int
  putStrLn "Enter the number of columns for the grid: "
  numCols <- readLn :: IO Int

  putStrLn $ "Enter the initial generation (" ++ show numRows ++ " rows, each containing " ++ show numCols ++ " characters 'O' or '.'): "
  initialGeneration <- replicateM numRows getLine
  let board = map (map (\c -> c == 'O')) initialGeneration
  playGame board
