-- Define the possible states of a cell
data CellState = Empty | Predator | Prey deriving (Eq, Show)

-- Type alias for a 2D grid of cell states
type Grid = [[CellState]]

-- Function to evolve a single cell based on its neighbors
evolveCell :: CellState -> [CellState] -> CellState
evolveCell Empty neighbors = Empty
evolveCell Predator neighbors
  | any (== Prey) neighbors = Predator  -- Predator eats prey if there is any nearby
  | otherwise = Empty  -- Otherwise, the predator moves randomly
evolveCell Prey neighbors
  | countEmptyNeighbors > 0 = Prey  -- Prey reproduces if there is an empty neighbor
  | otherwise = Empty  -- Otherwise, the prey moves randomly
  where
    countEmptyNeighbors = length $ filter (== Empty) neighbors
	
-- Function to evolve a single row of cells based on their neighbors
evolveRow :: [CellState] -> [CellState] -> [CellState]
evolveRow [] [] = []
evolveRow (currentState : rest) (neighbors : restNeighbors) =
  evolveCell currentState neighbors : evolveRow rest restNeighbors

-- Function to evolve the entire grid based on the given rules
evolveGrid :: Grid -> Grid
evolveGrid grid = zipWith evolveRow grid neighborsGrid
  where
    neighborsGrid = map (map (\(x, y) -> (grid !! y) !! x)) neighborIndices
    neighborIndices =
      [ [ (x + dx, y + dy)
        | dx <- [-1 .. 1]
        , dy <- [-1 .. 1]
        , not (dx == 0 && dy == 0)
        , x + dx >= 0 && x + dx < width
        , y + dy >= 0 && y + dy < height
        ]
      | x <- [0 .. width - 1]
      , y <- [0 .. height - 1]
      ]
    width = length (head grid)
    height = length grid

-- Function to display the grid
showGrid :: Grid -> String
showGrid = unlines . map (map showCell)
  where
    showCell Empty = "."
    showCell Predator = "X"
    showCell Prey = "O"

-- Main simulation function
simulate :: Grid -> Int -> IO ()
simulate grid generations = do
  let finalGrid = iterate evolveGrid grid !! generations
  putStrLn $ showGrid finalGrid

main :: IO ()
main = do
  putStrLn "Enter the initial grid (use '.' for Empty, 'X' for Predator, and 'O' for Prey):"
  initialGrid <- readGrid
  putStrLn "Enter the number of generations:"
  generationsStr <- getLine
  let generations = read generationsStr
  simulate initialGrid generations

-- Helper function to read the grid from user input
readGrid :: IO Grid
readGrid = do
  lines <- sequence $ replicate 10 getLine  -- Adjust the grid size (10x10 in this case)
  return $ map parseLine lines
  where
    parseLine line = map parseCell line
    parseCell '.' = Empty
    parseCell 'X' = Predator
    parseCell 'O' = Prey
    parseCell _ = Empty  -- Treat any other character as Empty
