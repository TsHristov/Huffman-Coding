import Data.List as List (sortBy, nub)
import qualified Data.Map as Map

type Input = [(Char, Int)]
type Code  = String
type CharFrequency = (Char, Int)
type HuffmanTree   = BinaryTree CharFrequency

data BinaryTree a = EmptyTree | Node { root  :: a,
                                       left  :: (BinaryTree a),
                                       right :: (BinaryTree a)
                                      } deriving (Show, Eq)

instance Functor BinaryTree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node root left right) = Node (f root) (fmap f left) (fmap f right)

instance Foldable BinaryTree where
  foldr f z EmptyTree = z
  foldr f z (Node root left right) = foldr f (f root (foldr f z right)) left

makeLeaf ::  CharFrequency -> HuffmanTree
makeLeaf root = (Node root EmptyTree EmptyTree)

isLeaf :: HuffmanTree -> Bool
isLeaf (Node root EmptyTree EmptyTree) = True
isLeaf                              _  = False

emptyTree :: HuffmanTree -> Bool
emptyTree EmptyTree = True
emptyTree         _ = False

makeTree :: CharFrequency -> HuffmanTree -> HuffmanTree -> HuffmanTree
makeTree root left right = Node root left right

char :: HuffmanTree -> Char
char = fst . root

frequency :: HuffmanTree -> Int
frequency = snd . root

charFrequency :: HuffmanTree -> HuffmanTree -> Ordering
charFrequency (Node (_, x) _ _) (Node (_, y) _ _)
  | x < y     = LT
  | x > y     = GT
  | otherwise = EQ

contentsFrequency :: String -> [CharFrequency]
contentsFrequency contents = nub $ map (\x -> (x, count x contents)) contents
  where count x l = sum $ map (\y -> if y == x then 1 else 0) l

-- Make each (Char,Frequency) pair a leaf in the Huffman Tree:
leaves :: Input -> [HuffmanTree]
leaves = map (\(x,y) -> makeLeaf (x, y))

-- Merge two trees together, so that their frequencies sum up:
merge :: HuffmanTree -> HuffmanTree -> HuffmanTree
merge left right = makeTree (' ', frequenciesSum) left right
  where frequenciesSum = frequency left + frequency right

-- Encode the data into a Huffman Tree:
code :: Input -> HuffmanTree
code input  = encode (leaves input)
  where encode []     = EmptyTree
        encode [tree] = tree
        encode input  = encode added
          where sorted      = sortBy charFrequency input 
                smallestTwo = take 2 sorted 
                merged      = merge (head smallestTwo) (head $ tail smallestTwo)
                removed     = filter (\x -> x /= (head smallestTwo) && x /= (head $ tail smallestTwo)) sorted
                added       = merged : removed

-- Map chars to their respective unique codes in the tree:
charCodes :: HuffmanTree -> [(Char, Code)]
charCodes huffmanTree = generateCodes huffmanTree ""
  where generateCodes huffmanTree path
          | isLeaf huffmanTree = [(char huffmanTree, path)]
          | otherwise          = generateCodes (left huffmanTree) (path ++ "0")
                                 ++ generateCodes (right huffmanTree) (path ++ "1")

-- Encoded original input:
encodeContents :: String -> Code
encodeContents text       = encoded
  where codes             = Map.fromList $ charCodes $ code $ contentsFrequency text
        encoded           = concat $ Prelude.map (\char -> getValue (Map.lookup char codes)) text
        getValue (Just a) = a

decode :: Code -> HuffmanTree -> String
decode code huf = helper code huf
  where helper (x:xs) tree
          | isLeaf tree = (char tree) : helper xs huf
          | emptyTree tree = ""
          | x == '0'   = helper xs (left tree)
          | x == '1'   = helper xs (right tree)

  
