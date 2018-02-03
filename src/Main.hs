module Main where

import HuffmanTree
import qualified Data.Binary as Binary

main :: IO ()
main = do
  putStrLn "Enter a file to compress: "
  file <- getLine
  contents <- readFile file
  let encodedContents = encode contents
  serialize encodedContents file
  (code,tree) <- deserialize file
  putStrLn "Original contents: "
  putStrLn contents
  putStr "Encoded contents: "
  putStrLn code
  putStr "Decoded original contents: "
  putStrLn $ (decode (code,tree))

serialize :: (Code, HuffmanTree) -> FilePath -> IO ()
serialize (code,tree) file = Binary.encodeFile (file ++ ".huffman") (code,tree)

deserialize :: FilePath -> IO (Code, HuffmanTree)
deserialize file = Binary.decodeFile (file ++ ".huffman")



