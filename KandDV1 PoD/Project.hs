import Control.Exception
import Prelude hiding(catch)
import System.Random
import qualified Data.Map as M
import Data.List.Split


--splitOn "," "my,comma,separated,list"
-- ["my","comma","separated","list"]
{- Game State
  Represents the number of stones in the three piles.
  Invariant: each element of the tuple >= 0.
-}
type GameState = [Colour]

type Ansicode = String 

-- type HashTable k v = H.CuckooHashTable k v

data Colour = Red | Green | Yellow | Blue | Magnenta | Cyan | White | Gray deriving (Show, Eq, Ord)

main = do
    key <- genKey
    print(key)
    key2 <- genKey
    print(key2) -- same as key
    print(victory [Magnenta,Green,Blue,Magnenta] key)


genKey :: IO GameState
genKey = do
    gen <- getStdGen -- At the moment, you have to reload the file in order for this to yield different numbers
    let (fstC, rnum1) = randomR (1,6) gen :: (Int, StdGen)
    let (sndC, rnum2) = randomR (1,6) rnum1 :: (Int, StdGen)
    let (trdC, rnum3) = randomR (1,6) rnum2 :: (Int, StdGen)
    let (frtC, _) = randomR (1,6) rnum3 :: (Int, StdGen)
    return $ map (toColour coloursInt) [fstC,sndC,trdC,frtC] 

coloursInt = createMapTable [1,2,3,4,5,6] [Red,Green,Yellow,Blue,Magnenta,Cyan] M.empty
coloursChar = createMapTable ['r','g','y','b','m','c'] [Red,Green,Yellow,Blue,Magnenta,Cyan] M.empty
ansicodeColours = createMapTable [Red,Green,Yellow,Blue,Magnenta,Cyan,White,Gray] ["\ESC[31;1mO\ESC[0m", "\ESC[32;1mO\ESC[0m", "\ESC[33;1mO\ESC[0m", "\ESC[34;1mO\ESC[0m", "\ESC[35;1mO\ESC[0m", "\ESC[36;1mO\ESC[0m", "\ESC[37;1mO\ESC[0m", "\ESC[90;1mO\ESC[0m"] M.empty :: M.Map Colour Ansicode

-- Don't know if this is MapTable or HashTable?
createMapTable ::(Ord a, Ord b) => [a] -> [b] ->  M.Map a b -> M.Map a b
createMapTable [x] [c] m = M.insert x c m
createMapTable (x:xs) (y:ys) m = M.insert x y (createMapTable xs ys m)

toColour :: (Ord a) =>  M.Map a Colour -> a -> Colour
toColour m n = col
    where
        (Just col) = M.lookup n m

toAnsicode :: Colour -> Ansicode
toAnsicode n = col
    where
        (Just col) = M.lookup n ansicodeColours

playerMove = do
    putStrLn "Guess the colours:"
    guess <- getLine
    let clrs = map (toColour coloursChar) guess
    -- let clrs = splitOn " " $ read guess
    return clrs
-- splitOn " " "r b c v" == ["r","b","c","v"]

-- readGuess :: [String] -> GameState -- needed in some way or another if GameState is not a list but a tuple 
    -- or if the input of the guess is not just rgcm
-- readGuess guess = map (toColour coloursChar) guess

{-
compareGuessKey [Blue,Green,Green,Green] [Red,Blue,Magnenta,Magnenta] == [White]
-}

compareGuessKey :: GameState -> GameState -> [Colour]
compareGuessKey guess key = rSpot ++ (wrongSpot wGuess wKey)
    where
        (rSpot,wGuess,wKey) = rightSpot guess key ([],[],[])

rightSpot [] [] (r,wg,wk) = (r,wg, wk)
rightSpot (g:gs) (k:ks) (r,wg,wk) = 
    if g == k
        then rightSpot gs ks ((Gray:r),wg,wk)
        else rightSpot gs ks (r,(g:wg),(k:wk))


wrongSpot [] _ = []
wrongSpot (g:gs) k =
    if elem g k 
        then (White : wrongSpot gs ks)
        else wrongSpot gs k 
    where 
        ks = removeG k g
        removeG (k:ks) g = 
            if k == g 
                then ks 
                else k : (removeG ks g)


victory :: GameState -> GameState -> Bool -- should probably be better if this is when all four are "right spot"?
victory guess key = guess == key 


{-
-----------------       -----------
| O | O | O | O |       | O O O O |
-----------------       -----------
-}

-- displayGameState 
-- could be hashtable with 4*8 + 4*8 (first for the game, second for the right and wrong spots) 
--spots that originally are only default from the beginning, then change default to the guessed colours and
-- to the right and wrong (or nothing) spots dots

