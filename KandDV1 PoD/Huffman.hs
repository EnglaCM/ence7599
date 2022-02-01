
-- DO NOT MODIFY THE FOLLOWING LINES
module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, encode, compress, decompress) where

import Table
import PriorityQueue
import Test.HUnit

{- a bit code (of a character or string) is represented by a list of Booleans
   INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some 
Huffman tree
 -}
type BitCode = [Bool]
-- END OF DO NOT MODIFY ZONE
--------------------------------------------------------------------------------
{- characterCounts s
   RETURNS: a table that maps each character that occurs in s to the number of times the character occurs in s
   EXAMPLES: characterCounts "this is an example of a huffman tree" ==
T [('i',2),('s',2),('x',1),('p',1),('l',1),('o',1),('h',2),('u',1),('f',3),('m',2),('a',4),('n',2),(' ',7),('t',2),('r',1),('e',4)]
            characterCounts ""  == T []
 -}
characterCounts :: String -> Table Char Int
characterCounts s = characterCounts' s (Table.empty)

{- characterCounts' s t
   RETURNS: table t with each character that occurs in s added and mapped to the number of times the character occurs in s
   EXAMPLES: characterCounts' "this is an example of a huffman tree" Table.empty == T [('i',2),('s',2),('x',1),('p',1),('l',1),('o',1),('h',2),('u',1),('f',3),('m',2),('a',4),('n',2),(' ',7),('t',2),('r',1),('e',4)]
            characterCounts' "" Table.empty == T []
 -}
characterCounts' :: String -> Table Char Int -> Table Char Int
characterCounts' [] t = t
characterCounts' (s:ss) t 
  | Table.exists t s = characterCounts' ss (Table.insert t' s (v+1))
  | otherwise = characterCounts' ss (Table.insert t s 1)
  where 
    t' = Table.delete t s 
    Just v = Table.lookup t s

testTable = characterCounts "this is an example of a huffman tree"
  
-- modify and add comments as needed
{- a HuffmanTree is a full binary tree where 
Each Leaf (c,i) is a unique character c and its count i
Each sub-tree's label is the count of all characters in it
INVARIANT: A sub-tree's character counts is never smaller than a lower-level-sub-trees character counts 
-}
-- data HuffmanTree = Leaf (Char, Int)
--                 | Node HuffmanTree Int HuffmanTree
data HuffmanTree = Leaf (Char, Int)    
                  | HuffmanNode HuffmanTree Int HuffmanTree deriving (Show)

{- huffmanTree t
   PRE:  t maps each key to a positive value
   RETURNS: a Huffman tree based on the character counts in t
   EXAMPLES: huffmanTree testTable == HuffmanNode (HuffmanNode (HuffmanNode (Leaf ('a',4)) 8 (HuffmanNode (Leaf ('t',2)) 4 (HuffmanNode (Leaf ('x',1)) 2 (Leaf ('r',1))))) 16 (HuffmanNode (HuffmanNode (Leaf ('h',2)) 4 (Leaf ('s',2))) 8 (HuffmanNode (Leaf ('i',2)) 4 (HuffmanNode (Leaf ('l',1)) 2 (Leaf ('p',1)))))) 36 (HuffmanNode (HuffmanNode (HuffmanNode (Leaf ('m',2)) 4 (HuffmanNode (Leaf ('o',1)) 2 (Leaf ('u',1)))) 8 (Leaf ('e',4))) 20 (HuffmanNode (HuffmanNode (Leaf ('n',2)) 5 (Leaf ('f',3))) 12 (Leaf (' ',7))))
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t = huffmanTree' (Table.iterate t prioritise PriorityQueue.empty)


{- prioritise p k 
Inserts the tuple k as Leaf k into the PriorityQueue p
EXAMPLES:
-}
prioritise :: PriorityQueue HuffmanTree -> (Char, Int) -> PriorityQueue HuffmanTree
prioritise p (k,v) = PriorityQueue.insert p (Leaf (k,v),v)

huffmanTree' :: PriorityQueue HuffmanTree -> HuffmanTree
huffmanTree' treeQueue
  | PriorityQueue.is_empty hs = t1
  | otherwise = huffmanTree' $ PriorityQueue.insert hss ((mergeTree p1 p2), (v1+v2))
  where
    (p1@(t1,v1),hs) = PriorityQueue.least treeQueue
    (p2@(t2,v2),hss) = PriorityQueue.least hs

mergeTree :: (HuffmanTree, Int) -> (HuffmanTree, Int) -> HuffmanTree
mergeTree (t1,v1) (t2,v2) = HuffmanNode t1 (v1+v2) t2

pempty = PriorityQueue.empty :: PriorityQueue HuffmanTree
heap = Table.iterate testTable prioritise pempty -- == BinoHeap [Node 4 1 'x' [Node 3 1 'r' [Node 2 2 'm' [Node 1 2 'n' [Node 0 4 'a' []],Node 0 3 'f' []],Node 1 2 't' [Node 0 7 ' ' []],Node 0 4 'e' []],Node 2 1 'l' [Node 1 1 'u' [Node 0 2 'h' []],Node 0 1 'o' []],Node 1 2 'i' [Node 0 2 's' []],Node 0 1 'p' []]]
-- == BinoHeap [Node 4 1 (Leaf ('x',1)) [Node 3 1 (Leaf ('r',1)) [Node 2 2 (Leaf ('m',2)) [Node 1 2 (Leaf ('n',2)) [Node 0 4 (Leaf ('a',4)) []],Node 0 3 (Leaf ('f',3)) []],Node 1 2 (Leaf ('t',2)) [Node 0 7 (Leaf (' ',7)) []],Node 0 4 (Leaf ('e',4)) []],Node 2 1 (Leaf ('l',1)) [Node 1 1 (Leaf ('u',1)) [Node 0 2 (Leaf ('h',2)) []],Node 0 1 (Leaf ('o',1)) []],Node 1 2 (Leaf ('i',2)) [Node 0 2 (Leaf ('s',2)) []],Node 0 1 (Leaf ('p',1)) []]]


order h
  | PriorityQueue.is_empty h = []
  | otherwise = l : order hs
  where
    (l, hs) = PriorityQueue.least h
-- order heap == [('x',1),('r',1),('l',1),('p',1),('u',1),('o',1),('i',2),('m',2),('h',2),('n',2),('t',2),('s',2),('f',3),('e',4),('a',4),(' ',7)]
-- == [(Leaf ('x',1),1),(Leaf ('r',1),1),(Leaf ('l',1),1),(Leaf ('p',1),1),(Leaf ('u',1),1),(Leaf ('o',1),1),(Leaf ('i',2),2),(Leaf ('m',2),2),(Leaf ('h',2),2),(Leaf ('n',2),2),(Leaf ('t',2),2),(Leaf ('s',2),2),(Leaf ('f',3),3),(Leaf ('e',4),4),(Leaf ('a',4),4),(Leaf (' ',7),7)]

testTree = huffmanTree testTable

{- codeTable h
   RETURNS: a table that maps each character in h to its Huffman code
   EXAMPLES:
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = codeTable' h Table.empty []

codeTable' :: HuffmanTree -> Table Char BitCode -> BitCode -> Table Char BitCode
codeTable' (Leaf (c,i)) t [] =  iTimes t c i
codeTable' (Leaf (c,i)) t b =  Table.insert t c (reverse b)
codeTable' (HuffmanNode t1 _ t2) t b = (codeTable' t2 (codeTable' t1 t (False:b))  (True:b))

iTimes :: Table Char BitCode -> Char -> Int -> Table Char BitCode
iTimes t c 1 = Table.insert t c [False]
iTimes t c n = iTimes (Table.insert t c [False]) c (n-1)


{- encode h s
   PRE: All characters in s appear in h
   RETURNS: the concatenation of the characters of s encoded using the Huffman code
table of h.
   EXAMPLES: encode testTree "tree" == [False,False,True,False,False,False,True,True,True,True,False,True,True,False,True]
            encode testTree "" == []
 -}
encode :: HuffmanTree -> String -> BitCode
encode h s = encode' (codeTable h) s []

encode' :: Table Char BitCode -> String -> BitCode -> BitCode
encode' t [] b = b
encode' t (s:ss) b = encode' t ss (b ++ bcode)
  where Just bcode = Table.lookup t s

{- compress s
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress s = (tree, bcode)
  where
    tree = huffmanTree $ characterCounts s
    bcode = encode tree s


{- decompress h bits
   PRE:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLES:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress h b = decompress' h b ""

decompress' :: HuffmanTree -> BitCode -> String -> String
decompress' h [] s = reverse s 
decompress' h@(Leaf (c,i)) (b:bs) s = decompress' h bs (ss:s)
  where 
    (_,ss) = (treeLookup h (b:bs))
decompress' h b s = decompress' h bs (ss:s)
  where
    (bs,ss) = (treeLookup h b)

treeLookup :: HuffmanTree -> BitCode -> (BitCode, Char)
treeLookup (Leaf (c,i)) b = (b,c)
treeLookup (HuffmanNode t1 _ _) (False:bs) = treeLookup t1 bs 
treeLookup (HuffmanNode _ _ t2) (True:bs) = treeLookup t2 bs 

t = "this is an example of a huffman tree"
--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------
-- characterCounts

test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')
-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))
-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))
-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)
test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)
test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)
-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]
