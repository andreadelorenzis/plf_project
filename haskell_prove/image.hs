import Data.Char (toLower, isLetter)
import System.IO

-- Tabella di traduzione delle lettere (codifica e decodifica)
translateTable :: [(Char, Char)]
translateTable = zip ['A'..'Z'] ['Z','Y'..'A'] ++ zip ['a'..'z'] ['z','y'..'a']

-- Funzione di codifica di una singola lettera
codificaLettera :: Char -> Char
codificaLettera c
  | isLetter c = case lookup (toLower c) translateTable of
                   Just t  -> if isLower c then t else toUpper t
                   Nothing -> c
  | otherwise  = c

-- Funzione di decodifica di una singola lettera
decodificaLettera :: Char -> Char
decodificaLettera c
  | isLetter c = case lookup (toLower c) (map (\(x,y) -> (y,x)) translateTable) of
                   Just t  -> if isLower c then t else toUpper t
                   Nothing -> c
  | otherwise  = c

-- Funzione per leggere il contenuto del file e applicare la trasformazione
trasformaFile :: (Char -> Char) -> FilePath -> FilePath -> IO ()
trasformaFile transform inFile outFile = do
  content <- readFile inFile
  let transformedContent = map transform content
  writeFile outFile transformedContent

-- Funzione per codificare il contenuto del file
codificaFile :: FilePath -> IO ()
codificaFile inFile = trasformaFile codificaLettera inFile "codificato.txt"

-- Funzione per decodificare il contenuto del file
decodificaFile :: FilePath -> IO ()
decodificaFile inFile = trasformaFile decodificaLettera inFile "decodificato.txt"

-- Funzione main per interagire con l'utente
main :: IO ()
main = do
  putStrLn "Inserisci il nome del file da codificare:"
  inFile <- getLine
  codificaFile inFile
  putStrLn "File codificato e scritto in codificato.txt."
  
  putStrLn "Inserisci il nome del file da decodificare:"
  inFile2 <- getLine
  decodificaFile inFile2
  putStrLn "File decodificato e scritto in decodificato.txt."
