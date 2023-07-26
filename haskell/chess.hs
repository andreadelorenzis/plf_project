-- Full Chess Game in Haskell

import Data.Char (toLower)

data Color = White | Black deriving (Eq, Show)
data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Square = EmptySquare | OccupiedSquare Color Piece deriving (Eq, Show)
type Board = [[Square]]
type Position = (Int, Int)
data Move = Move Position Position deriving (Eq, Show)

-- Function to initialize an empty board
emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 EmptySquare)

-- Function to initialize the starting chess board
startingBoard :: Board
startingBoard =
  [ [OccupiedSquare Black Rook, OccupiedSquare Black Knight, OccupiedSquare Black Bishop, OccupiedSquare Black Queen, OccupiedSquare Black King, OccupiedSquare Black Bishop, OccupiedSquare Black Knight, OccupiedSquare Black Rook]
  , replicate 8 (OccupiedSquare Black Pawn)
  , replicate 8 EmptySquare
  , replicate 8 EmptySquare
  , replicate 8 EmptySquare
  , replicate 8 EmptySquare
  , replicate 8 (OccupiedSquare White Pawn)
  , [OccupiedSquare White Rook, OccupiedSquare White Knight, OccupiedSquare White Bishop, OccupiedSquare White Queen, OccupiedSquare White King, OccupiedSquare White Bishop, OccupiedSquare White Knight, OccupiedSquare White Rook]
  ]

-- Function to apply a move on the board
applyMove :: Board -> Move -> Board
applyMove board (Move fromPos toPos) =
  let fromSquare = getSquare board fromPos
      updatedBoard = setSquare board fromPos EmptySquare
  in setSquare updatedBoard toPos fromSquare

-- Helper function to get the piece at a specific position
getSquare :: Board -> Position -> Square
getSquare board (row, col) = board !! row !! col

-- Helper function to set a specific square on the board
setSquare :: Board -> Position -> Square -> Board
setSquare board (row, col) square =
  take row board ++ [take col (board !! row) ++ [square] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board

-- Function to check if a move is valid according to the chess rules
isValidMove :: Board -> Color -> Move -> Bool
isValidMove board color (Move fromPos toPos) =
  let fromSquare = getSquare board fromPos
      toSquare = getSquare board toPos
      isValidPieceMove =
        case fromSquare of
          OccupiedSquare pieceColor piece ->
            color == pieceColor &&
            (isValidPieceMove' piece fromPos toPos board)
          EmptySquare -> False
  in isValidPieceMove

isValidPieceMove' :: Piece -> Position -> Position -> Board -> Bool
-- Implement piece-specific move validation logic here
isValidPieceMove' piece fromPos toPos board = True -- Placeholder

-- Function to parse user input into a Move
parseMove :: String -> Maybe Move
parseMove input = -- Implement parsing logic here
  -- Example parsing logic (simple format: e2 e4)
  let positions = words input
  in case positions of
    [from, to] -> do
      fromPos <- parsePosition from
      toPos <- parsePosition to
      return $ Move fromPos toPos
    _ -> Nothing

parsePosition :: String -> Maybe Position
parsePosition pos =
  case pos of
    [file, rank] -> do
      col <- elemIndex file files
      row <- elemIndex (toLower rank) ranks
      return (row, col)
    _ -> Nothing
  where
    files = ['a'..'h']
    ranks = ['1'..'8']
    elemIndex x xs = case dropWhile (/= x) xs of
                       [] -> Nothing
                       idx -> Just (length xs - length idx)

-- Function to switch to the other player's turn
nextPlayer :: Color -> Color
nextPlayer White = Black
nextPlayer Black = White

-- Function to display the current board
printBoard :: Board -> IO ()
printBoard board = mapM_ printRow board
  where
    printRow row = putStrLn $ concatMap showSquare row
    showSquare EmptySquare = ". "
    showSquare (OccupiedSquare White piece) = show piece ++ " "
    showSquare (OccupiedSquare Black piece) = map toLower (show piece) ++ " "

-- Main game loop
playChess :: Board -> Color -> IO ()
playChess board currentPlayer = do
  printBoard board
  putStrLn $ show currentPlayer ++ "'s turn. Enter your move (e.g., 'e2 e4'):"
  input <- getLine
  case parseMove input of
    Just move ->
      if isValidMove board currentPlayer move
        then do
          let updatedBoard = applyMove board move
          playChess updatedBoard (nextPlayer currentPlayer)
        else do
          putStrLn "Invalid move! Try again."
          playChess board currentPlayer
    Nothing -> do
      putStrLn "Invalid input! Try again."
      playChess board currentPlayer

main :: IO ()
main = do
  putStrLn "Welcome to Haskell Chess!"
  playChess startingBoard White
