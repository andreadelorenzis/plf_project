{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)

data Color = White | Black deriving (Eq, Show)
data Piece = King | Queen | Bishop | Knight | Rook | Pawn deriving (Eq, Show)
type Position = (Int, Int)
type Board = Map.Map Position (Color, Piece)

data GameState = GameState
    { board :: Board
    , currentPlayer :: Color
    , gameWon :: Bool
    }

main :: IO ()
main = do
    putStrLn "Welcome to Haskell Chess!"
    let initialState = GameState startingBoard White False
    playGame initialState

startingBoard :: Board
startingBoard = Map.fromList
    [ ((1, 1), (White, Rook))
    , ((1, 2), (White, Knight))
    , ((1, 3), (White, Bishop))
    , ((1, 4), (White, Queen))
    , ((1, 5), (White, King))
    , ((1, 6), (White, Bishop))
    , ((1, 7), (White, Knight))
    , ((1, 8), (White, Rook))
    , ((2, 1), (White, Pawn))
    , ((2, 2), (White, Pawn))
    , ((2, 3), (White, Pawn))
    , ((2, 4), (White, Pawn))
    , ((2, 5), (White, Pawn))
    , ((2, 6), (White, Pawn))
    , ((2, 7), (White, Pawn))
    , ((2, 8), (White, Pawn))
    , ((7, 1), (Black, Pawn))
    , ((7, 2), (Black, Pawn))
    , ((7, 3), (Black, Pawn))
    , ((7, 4), (Black, Pawn))
    , ((7, 5), (Black, Pawn))
    , ((7, 6), (Black, Pawn))
    , ((7, 7), (Black, Pawn))
    , ((7, 8), (Black, Pawn))
    , ((8, 1), (Black, Rook))
    , ((8, 2), (Black, Knight))
    , ((8, 3), (Black, Bishop))
    , ((8, 4), (Black, Queen))
    , ((8, 5), (Black, King))
    , ((8, 6), (Black, Bishop))
    , ((8, 7), (Black, Knight))
    , ((8, 8), (Black, Rook))
    ]

playGame :: GameState -> IO ()
playGame state
    | gameWon state = endGame state
    | otherwise = do
        printBoard $ board state
        putStrLn $ show (currentPlayer state) ++ "'s turn."
        putStrLn "Enter the position of the piece you want to move (e.g., A2):"
        fromInput <- getLine
        putStrLn "Enter the destination position (e.g., B4):"
        toInput <- getLine
        let maybeMove = parseMove fromInput toInput
        case maybeMove of
            Just move -> do
                let nextState = applyMove move state
                playGame nextState
            Nothing -> do
                putStrLn "Invalid move!"
                playGame state

parseMove :: String -> String -> Maybe (Position, Position)
parseMove from to = do
    fromPos <- parsePosition from
    toPos <- parsePosition to
    return (fromPos, toPos)

parsePosition :: String -> Maybe Position
parsePosition [col, row] = do
    let colIndex = Char.ord (Char.toUpper col) - Char.ord 'A' + 1
        rowIndex = Char.digitToInt row
    if colIndex >= 1 && colIndex <= 8 && rowIndex >= 1 && rowIndex <= 8
        then return (rowIndex, colIndex)
        else Nothing
parsePosition _ = Nothing

applyMove :: (Position, Position) -> GameState -> GameState
applyMove (fromPos, toPos) state
    | isJust $ Map.lookup fromPos (board state) = nextState
    | otherwise = state
  where
    currentPiece = fromJust $ Map.lookup fromPos (board state)
    nextState = state { board = Map.insert toPos currentPiece $ Map.delete fromPos $ board state }

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn "   A B C D E F G H"
    putStrLn "  -----------------"
    mapM_ printRow [8, 7 .. 1]
    putStrLn "  -----------------"
  where
    printRow row = do
        putStr (show row ++ "|")
        mapM_ (\col -> putStr [' ', cell (row, col), ' ']) [1..8]
        putStrLn "|"
    cell pos = case Map.lookup pos board of
        Just (White, piece) -> pieceToChar piece
        Just (Black, piece) -> Char.toLower (pieceToChar piece)
        Nothing -> ' '

pieceToChar :: Piece -> Char
pieceToChar King = 'K'
pieceToChar Queen = 'Q'
pieceToChar Bishop = 'B'
pieceToChar Knight = 'N'
pieceToChar Rook = 'R'
pieceToChar Pawn = 'P'

endGame :: GameState -> IO ()
endGame state
    | gameWon state = putStrLn $ show (currentPlayer state) ++ " wins!"
    | otherwise = putStrLn "Unexpected game state."
