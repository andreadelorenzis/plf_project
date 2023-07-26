module Main where

import Data.Char (ord, chr)
import System.IO

shift :: Int
shift = 3

-- Function to encode the input text using Caesar cipher
encode :: String -> String
encode = map shiftChar
  where
    shiftChar c
      | 'a' <= c && c <= 'z' = shiftLower c
      | 'A' <= c && c <= 'Z' = shiftUpper c
      | otherwise = c
    shiftLower c = chr ((ord c - ord 'a' + shift) `mod` 26 + ord 'a')
    shiftUpper c = chr ((ord c - ord 'A' + shift) `mod` 26 + ord 'A')

-- Function to decode the encoded text using Caesar cipher
decode :: String -> String
decode = map shiftChar
  where
    shiftChar c
      | 'a' <= c && c <= 'z' = shiftLower c
      | 'A' <= c && c <= 'Z' = shiftUpper c
      | otherwise = c
    shiftLower c = chr ((ord c - ord 'a' - shift + 26) `mod` 26 + ord 'a')
    shiftUpper c = chr ((ord c - ord 'A' - shift + 26) `mod` 26 + ord 'A')

-- Function to read binary input from a file and write the result to another file
processFile :: (String -> String) -> FilePath -> FilePath -> IO ()
processFile f inputPath outputPath = do
  content <- withBinaryFile inputPath ReadMode hGetContents
  let result = f content
  withBinaryFile outputPath WriteMode $ \h -> hPutStr h result

main :: IO ()
main = do
  putStrLn "Choose an option:"
  putStrLn "1. Encode a file"
  putStrLn "2. Decode a file"
  option <- getLine
  case option of
    "1" -> do
      putStrLn "Enter the path of the file to be encoded:"
      inputPath <- getLine
      processFile encode inputPath "encoded.txt"
      putStrLn "File encoded successfully."
    "2" -> do
      putStrLn "Enter the path of the file to be decoded:"
      inputPath <- getLine
      processFile decode inputPath "decoded.txt"
      putStrLn "File decoded successfully."
    _ -> putStrLn "Invalid option. Please choose 1 or 2."
