module Main where

import HuffmanTree

main :: IO ()
main = do
  putStrLn "Enter a file to compress: "
  file <- getLine
  contents <- readFile file
  let encoded = encode contents
  serialize encoded file
  (code,tree) <- deserialize file :: IO (String, HuffmanTree)
  putStrLn "Original contents: "
  putStrLn contents
  putStr "Encoded contents: "
  putStrLn $ code
  putStr "Decoded original contents: "
  putStrLn $ (decode (code,tree))


