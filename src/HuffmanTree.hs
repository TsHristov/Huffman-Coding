module HuffmanTree where

import Data.List as List (sortBy, nub, foldl')
import qualified Data.Binary as Binary
import qualified Data.Map as Map

type Code            = String
type CharFrequency   = (Char, Int)
type CharFrequencies = [CharFrequency]

data HuffmanTree = EmptyTree | Node { root  :: CharFrequency,
                                      left  :: HuffmanTree,
                                      right :: HuffmanTree
                                    } deriving (Show, Eq)

-- | Make HuffmanTree an instance of Ord, so we can sort trees based on char frequencies:
instance Ord HuffmanTree where
  compare (Node (_,x) _ _) (Node (_,y) _ _) = compare x y

-- | Make HuffmanTree an instance of Binary, so it can be serialized:
instance Binary.Binary HuffmanTree where
  put EmptyTree = do Binary.put (0 :: Binary.Word8)
  put (Node root left right) = do Binary.put (1 :: Binary.Word8)
                                  Binary.put root
                                  Binary.put left
                                  Binary.put right
  get = do t <- Binary.get :: Binary.Get Binary.Word8
           case t of
             0 -> do return EmptyTree
             1 -> do root <- Binary.get
                     left <- Binary.get
                     right <- Binary.get
                     return (Node root left right)

makeLeaf ::  CharFrequency -> HuffmanTree
makeLeaf x = (Node x EmptyTree EmptyTree)

leaf :: HuffmanTree -> Bool
leaf (Node _ EmptyTree EmptyTree) = True
leaf                           _  = False

-- | Get character associated with root:
char :: HuffmanTree -> Char
char = fst . root

-- | Get frequency associated with root:
frequency :: HuffmanTree -> Int
frequency = snd . root

-- | Obtain file`s characters` frequencies:
-- | Example: contentsFrequency "abracadabra" -> [('a',5),('b',2),('r',2),('c',1),('d',1)]
contentsFrequencies :: String -> [CharFrequency]
contentsFrequencies contents = nub $ map (\x -> (x, (count x contents))) contents
  where count x              = foldl' (\acc y -> if y == x then acc + 1 else 0) 0 

-- | Make each (Char,Frequency) pair a leaf in the Huffman Tree:
makeLeaves :: CharFrequencies -> [HuffmanTree]
makeLeaves = map makeLeaf

-- | Merge two trees together, so that their frequencies sum up:
mergeTrees :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeTrees  leftTree rightTree = Node (' ', frequenciesSum) leftTree rightTree
  where frequenciesSum         = frequency leftTree + frequency rightTree

-- | Construct HuffmanTree based on file`s unique characters and their respective frequencies:
constructHuffmanTree :: CharFrequencies -> HuffmanTree
constructHuffmanTree          = construct . makeLeaves
  where construct []          = EmptyTree
        construct [tree]      = tree
        construct tree        = construct updated
          where sorted        = sortBy compare tree 
                smallestTrees = take 2 sorted 
                rightTree     = head smallestTrees
                leftTree      = head $ tail smallestTrees
                merged        = mergeTrees rightTree leftTree
                removed       = drop 2 sorted
                updated       = merged : removed

-- | Generate unique binary indentification codes per file`s unique characters:
binaryCodes :: HuffmanTree -> [(Char, Code)]
binaryCodes = generateCodes ""
  where generateCodes path tree 
          | leaf tree   = [(char tree, path)]
          | otherwise   = generateCodes (path ++ "0") (left tree)
                          ++ generateCodes (path ++ "1") (right tree)

-- | Transform file`s contents to encoded sequence of 1`s and 0`s (Huffman Algorithm):
-- | Example: encodeContents "abracadabra" -> ("01111001100011010111100", <tree>),
-- |          where <tree> is a valid Huffman Tree.
encode :: String -> (Code, HuffmanTree)
encode content      = (encoded content, huffmanTree)
  where huffmanTree = constructHuffmanTree $ contentsFrequencies content
        encoded     = concat . binaryCode
          where getValue (Just x) = x
                codes             = Map.fromList $ binaryCodes $ huffmanTree
                binaryCode        = Prelude.map (\x -> getValue (Map.lookup x codes))

-- | Decode a Huffman Tree in order to obtain file`s initial contents:
decode :: (Code, HuffmanTree) -> String
decode (binaryCode, huffmanTree)                     = decodeTree binaryCode huffmanTree
  where decodeTree "" EmptyTree                      = ""
        decodeTree "" tree                           = [char tree]
        decodeTree code (Node x EmptyTree EmptyTree) = fst x : decodeTree code huffmanTree
        decodeTree (x:xs) tree                       = if x == '0'
                                                       then decodeTree xs (left tree)
                                                       else decodeTree xs (right tree)
