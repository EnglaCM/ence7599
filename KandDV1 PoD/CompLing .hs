-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS

--1
{- wordAcc WordTally word
Checks if word is in WordTally. If that is the case, it counts +1 for the word, otherwise it adds the word with a count of 1.
RETURNS: WordTally, where word is in the WordTally
EXAMPLES: wordAcc [("a", 1), ("dog",3)] "a" = [("a", 2), ("dog",3)]
          wordAcc [("a", 1), ("dog",3)] "big" = [("a", 1), ("dog",3), ("big",1)]
-}
-- VARIANT: length (ws:wt)

wordAcc :: [(String, Int)] -> String -> [(String, Int)]
wordAcc [] s = [(s, 1)]
wordAcc (ws:wt) s 
  | s == word = (word, (acc+1)):wt
  | otherwise = (word, acc) : (wordAcc wt s)
  where (word, acc) = ws

{- wordFind WordTally Document
Finds each word in Document, and send it and the WordTally as arguments to wordAcc
RETURNS: A WordTally containing all words in Document
EXAMPLES: wordFind [] [] = []
          wordFind [] [["a","single","man","a","good"],["little","known"]] = [("a",2),("single",1),("man",1),("good",1),("little",1),("known",1)]
-}
-- VARIANT: length ((x:xs):y)

wordFind :: WordTally -> Document -> WordTally
wordFind wt [] = wt
wordFind wt ((x:[]):y) = wordFind (wordAcc wt x) y
wordFind wt ((x:xs):y) = wordFind (wordAcc wt x) ([xs] ++ y)

{- wordCount Document
Counts each word in Document, by using wordFind
RETURNS: All counted words in a WordTally
EXAMPLES: wordCount [] = []
          wordCount [["a","single","man","a","good"],["little","known"]] = [("a",2),("single",1),("man",1),("good",1),("little",1),("known",1)]
-}

wordCount :: Document -> WordTally
wordCount [] = []
wordCount doc = wordFind [] doc



---2
{- adjacentPairsSentence Sentence
Finds all adjecent pairs in Sentence
RETURNS: All adjecent pairs in Sentence
EXAMPLES: adjacentPairsSentence ["a"] = []
          adjacentPairsSentence ["a","single","man","a","good"] = [("a","single"),("single","man"),("man","a"),("a","good")]
-}
-- VARIANT: length (x:x2:xs)

adjacentPairsSentence :: Sentence -> Pairs
adjacentPairsSentence [x] = []
adjacentPairsSentence (x:x2:xs) = (x,x2) : adjacentPairsSentence (x2:xs)
{- adjacentPairs Document
Find all adjecent pairs in Document by using adjacentPairsSentence for each Sentence in Document
RETURNS: All adjecent pairs in Document
EXAMPLES: adjacentPairs ["a"] = []
          adjacentPairs [["a","single","man","a","good"],["little","known"]] = [("a","single"),("single","man"),("man","a"),("a","good"),("little","known")]
-}
-- VARIANT: length (x:xs)

adjacentPairs :: Document -> Pairs
adjacentPairs [] = []
adjacentPairs (x:xs) = adjacentPairsSentence x ++ adjacentPairs xs 


--3
{- initialPairsSentence Sentence
Find the first two words in Sentence
RETURNS: First two words in Sentence
EXAMPLES: initialPairsSentence ["a"] = []
          initialPairsSentence ["a","single","man","a","good"] = [("a","single")]
-}
initialPairsSentence :: Sentence -> Pairs
initialPairsSentence [x] = []
initialPairsSentence (x:x2:xs) = [(x,x2)]
{- initialPairs Document
Find the first pairs in each Sentence in Document
RETURNS: First two words in each Sentence in Document
EXAMPLES: initialPairs [["a"]] = []
          initialPairs [["a","single","man","a","good"],["little","known"]] = [("a","single"),("little","known")]
-}
-- VARIANT: length (x:xs)

initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs (x:xs) = initialPairsSentence x ++ initialPairs xs
{- finalPairsSentence Sentence
Find the last two words in Sentence
RETURNS: Last two words in Sentence
EXAMPLES: finalPairsSentence ["a"] = []
          finalPairsSentence ["a","single","man","a","good"] = [("a","good")]
-}
finalPairsSentence :: Sentence -> Pairs
finalPairsSentence [x] = []
finalPairsSentence x = [(y,ys)]
  where [y,ys] = drop ((length x)-2) x

{- finalPairs Document
Find the last two words in each Sentence in Document
RETURNS: last two words in each Sentence in Document
EXAMPLES: finalPairs [["a"]] = []
          finalPairs [["a","single","man","a","good"],["little","known"]] = [("a","good"),("little","known")]
-}
finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs (x:xs) = finalPairsSentence x ++ finalPairs xs

--4
{- pairsTallyCheck (word1, word2) PairsTally
Checks if any combination of word1 and word2 is in PairsTally. If that is the case, it counts +1 for the pair, otherwise it adds the pair to the PairsTally and count 1 for the pair.
RETURNS: PairsTally with (word1, word2) and their count in it
EXAMPLES: pairsTallyCheck ("a","wife") [(("good","a"),1),(("want","a"),3)] = [(("good","a"),1),(("want","a"),3),(("a","wife"),1)]
          pairsTallyCheck ("a","good") [(("good","a"),1),(("want","a"),3)] = [(("good","a"),2),(("want","a"),3)]
-}
-- VARIANT: length ((p, acc):ps)

pairsTallyCheck :: (String, String) -> PairsTally -> PairsTally
pairsTallyCheck (word1, word2) [] = [((word1, word2), 1)]
pairsTallyCheck (word1, word2) ((p, acc):ps) 
  | (word1, word2) == p || (word2, word1) == p = (p, (acc+1)):ps
  | otherwise = (p,acc) : pairsTallyCheck (word1, word2) ps

{- pairsFind Pairs PairsTally
Finds all pairs in Pairs, and uses pairsTallyCheck to create a PairsTally with them
RETURNS: PairsTally with all Pairs
EXAMPLES: pairsFind [] []  = []
          pairsFind [("a","good"),("a","wife")] [(("good","a"),2),(("want","a"),3)] = [(("good","a"),3),(("want","a"),3),(("a","wife"),1)]
-}
-- VARIANT: lenght (x:xs)

pairsFind :: Pairs -> PairsTally -> PairsTally
pairsFind [] pt = pt
pairsFind (x:xs) pt = pairsFind xs (pairsTallyCheck x pt)

{- pairsCount Pairs
Sends Pairs and an empty PairsTally to pairsFind
RETURNS: a PairsTally containing all Pairs
EXAMPLES: pairsCount [("a","good"),("a","wife"),("wife","a")]
= [(("a","good"),1),(("a","wife"),2)]
-}
pairsCount :: Pairs -> PairsTally
pairsCount pairslist = pairsFind pairslist []


--5
{- neighbours PairsTally word
Find all words that appear with word in the PairsTally, and the occurences
RETURNS: WordTally with the neighbouring words to word and the occurences
EXAMPLES: neighbours [(("a","good"),1),(("a","wife"),2)] "must"
= []
          neighbours [(("a","good"),1),(("a","wife"),2)] "a"
= [("good",1),("wife",2)]
-}
-- VARIANT: length (((word1,word2), acc):ps)

neighbours :: PairsTally -> String -> WordTally
neighbours [] word = []
neighbours (((word1,word2), acc):ps) word 
  | word == word1 = (word2, acc) : (neighbours ps word)
  | word == word2 = (word1, acc) : (neighbours ps word)
  | otherwise = neighbours ps word



---6
{- mostCommonNeighbourOcc WordTally occurences defaultString
Finds the neighbour in WordTally that has the highest occurences. If WordTally is empty, it will return defaultString
RETURNS: The (most common) neighbour to word (or defaultString)
EXAMPLES: mostCommonNeighbourOcc [("good",1),("wife",2)] 0 ""
= "wife"
          mostCommonNeighbourOcc [] 0 "" = ""
-}
-- VARIANT: lenght ((neighbour, occ):ps)

mostCommonNeighbourOcc :: WordTally ->  Int -> String -> String
mostCommonNeighbourOcc [] acc mcneighbour = mcneighbour
mostCommonNeighbourOcc ((neighbour, occ):ps) acc mcneighbour
  | acc > occ = mostCommonNeighbourOcc ps acc mcneighbour
  | otherwise = mostCommonNeighbourOcc ps occ neighbour

{- mostCommonNeighbour PairsTally word
Finds the most common neighbour to word in PairsTally
RETURNS: The most common neighbour - if there is one. Otherwise: Nothing
EXAMPLES: mostCommonNeighbour [(("a","good"),1),(("a","wife"),2)] "a"
= Just "wife"
          mostCommonNeighbour [(("a","good"),1),(("a","wife"),2)] "man" = Nothing
-}
mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour pt word
  | (neighbours pt word) == [] = Nothing
  | otherwise = Just (mostCommonNeighbourOcc (neighbours pt word) 0 "")




-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]]) 

test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])
                      
test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])
                      

-- pairsCount
test4 = TestCase $ assertBool "pairsCount simple" 
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test5 = TestCase $ assertBool "pairsCount tricky" 
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in 
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test6 = TestCase $ assertEqual "neighbours left" [("b",2)] 
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a") 

test7 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b") 

-- mostCommonNeighbour
test8 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun") 
                                                                  (mostCommonNeighbour input "the") 
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test9 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\"" 
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test10 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\"" 
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet") 

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]




