import Control.Exception
import Prelude hiding(catch)
import System.Random
import qualified Data.Map as M
import Data.List

data Colour = Red | Green | Yellow | Blue | Magnenta | Cyan | White | Gray deriving (Show, Eq, Ord)

type Ansicode = String 

type Mine = Bool

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

gameBoard mines = createGameBoard [1..256] mines M.empty  -- 16x16 game board


{-
Mine borde vara datatyp som antingen är Bool eller Int, Bool för (inga) mines, och int för hur många närliggande mines
För Mine i createGameBoard ska ju spara "det riktiga/dolda värdet" på plattan

index borde vara tuple  ((div x 16) + 1,mod x 16)  --------där vi räknar ut vilkwn som är mina om behövs
-}
createGameBoard :: [Int] -> [Int] ->  M.Map Int (Ansicode, Mine) -> M.Map Int (Ansicode, Mine)
createGameBoard [x] [] gameboard = M.insert x ("\ESC[37;1mO\ESC[0m", False)  gameboard
createGameBoard [x] _ gameboard = M.insert x ("\ESC[37;1mO\ESC[0m", True) gameboard
createGameBoard (x:xs) [] gameboard = M.insert x ("\ESC[37;1mO\ESC[0m", False) (createGameBoard xs [] gameboard)
createGameBoard (x:xs) (m:ms) gameboard = M.insert x ("\ESC[37;1mO\ESC[0m", x==m) (createGameBoard xs minesLeft gameboard) 
    where 
        minesLeft = 
            if x == m 
                then ms 
                else (m:ms)



genMines = do
    gen <- newStdGen :: IO StdGen
    return $ sort . take 40 . nub $ (randomRs (1,256) gen :: [Int])

readGameBoard start end row gameboard = 
    if start < end
        then ansi ++ " | " ++ readGameBoard (start+1) end row gameboard
        else ansi ++ " |    " ++ show row
    where 
        Just (ansi,mine) = M.lookup start gameboard

displayGameBoard gameboard = do
    putStrLn $ readGameBoard 1 16 1 gameboard
    putStrLn $ readGameBoard 17 32 2 gameboard
    putStrLn $ readGameBoard 33 48 3 gameboard
    putStrLn $ readGameBoard 49 64 4 gameboard
    putStrLn $ readGameBoard 65 80 5 gameboard
    putStrLn $ readGameBoard 81 96 6 gameboard
    putStrLn $ readGameBoard 97 112 7 gameboard
    putStrLn $ readGameBoard 113 128 8 gameboard
    putStrLn $ readGameBoard 129 144 9 gameboard
    putStrLn $ readGameBoard 145 160 10 gameboard
    putStrLn $ readGameBoard 161 176 11 gameboard
    putStrLn $ readGameBoard 177 192 12 gameboard
    putStrLn $ readGameBoard 193 208 13 gameboard
    putStrLn $ readGameBoard 209 224 14 gameboard
    putStrLn $ readGameBoard 225 240 15 gameboard
    putStrLn $ readGameBoard 241 256 16 gameboard
