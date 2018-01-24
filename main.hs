import System.IO
import HuffmanTree
                                
main = do
  putStrLn "Enter a file to compress: "
  file <- getLine
  contents <- readFile file
  let encoded = encode contents
  putStrLn "Original contents: "
  putStrLn $ contents
  putStr "Encoded contents: "
  putStrLn $ fst encoded
  putStr "Decoded original contents: "
  putStrLn $ (decode encoded)

