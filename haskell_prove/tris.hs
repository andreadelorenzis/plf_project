{- IMPORTAZIONE DELLE LIBRERIE -}
import Data.List
import Data.Maybe

{- Simbolo del giocatore -}
data Player = X | O deriving (Eq, Show)

{- Una cella può contenere un simbolo (Just Player) o essere 
   vuota (Nothing) -}
type Cell = Maybe Player

{- La tabella di gioco è una lista 2D -}
type Board = [[Cell]]

{- Crea una tabella di gioco vuota come una matrice 3x3 -}
emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

{- Funzione per visualizzare la tabella di gioco -}
displayBoard :: Board -> IO ()
displayBoard board do
  putStrln "    0   1   2    "
  putStrln "  +---+---+---+  "
  putStrln $ unlines $ zipWith (\i row -> show i ++ " | " ++ showRow row) [0..] board
  putStrln "  +---+---+---+  "
  where
    showRow row = intercalate " | " $ map showCell row
	showCell Nothing   = " "
	showCell (Just X) = "X"
	showCell (Just O) = "O"
	
{- Funzione che controlla se un giocatore ha vinto la partita -}
isWinner :: Player -> Board -> Bool
isWinner p b = any checkLine (rows ++ columns ++ diagonals)
  where
    checkLine line = all (== Just p) line
	rows = b
	columns = transpose b
	diagonals = [[b !! i !! i | i <- [0..2]], [b !! 1 !! (2 - i) | i <- [0..2]]]
	
{- Funzione che ontrolla se la tabella è piena (cioè se c'è una parità) -}
isDraw :: Board -> Bool
isDraw = all isJust . concat

{- Funzione che ottiene la lista di posizioni delle celle vuote -}
emptyCells :: Board -> [(Int, Int)]
emptyCells b = [(i, j) | i <- [0..2], j <- [0..2], isNothing (b !! i !! j)]

{- Funzione che posiziona una mossa del giocatore sulla tabella -}
makeMove :: Board -> (Int, Int) -> Player -> Board
makeMove b (i, j) p = replace2D i j (Just p) b
  where
    replace2D r c val xs = take r xs ++ [take c (xs !! r) ++ [val] ++ drop 
	(c + 1) (xs !! r)] ++ drop (r + 1) xs

{- Funzione che implementa il ciclo di gioco -}
playGame :: Player -> Board -> IO()


