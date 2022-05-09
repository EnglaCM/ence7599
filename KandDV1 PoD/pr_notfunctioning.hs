import Control.Exception
import Prelude hiding(catch)
import System.Random
import qualified Data.Map as M
import Data.List


type Ansicode = String 

type Mine = Bool

data Action = F | P deriving (Read,Eq)

data MineData = One | Two | Three | Four | Five | Six | Seven | Eight | Blank | Bomb | Flag | White deriving (Show,Eq,Ord)

{- Maps between MineData and the corresponding Ansicode-}
mineDataAnsi = createMapTable [One, Two, Three, Four, Five, Six, Seven, Eight, Blank, Bomb, Flag, White] ["\ESC[34;1m1\ESC[0m", "\ESC[32;1m2\ESC[0m", "\ESC[31;1m3\ESC[0m","\ESC[35;1m4\ESC[0m","\ESC[33;1m5\ESC[0m","\ESC[36;1m6\ESC[0m","\ESC[32;1m7\ESC[0m","\ESC[31;1mO\ESC[0m", " ", "\ESC[31;1mX\ESC[0m","\ESC[31;1m!\ESC[0m","\ESC[37;1mO\ESC[0m"] M.empty :: M.Map MineData Ansicode

{- createMapTable xs ys m
Inserts elements from xs as keys and elements from ys as values in m.
PRE: m is empty, length xs == length ys > 0
RETURNS M.Map x y where x <- xs and y <- ys
EXAMPLES: createMapTable [1,2,3] ['a','b','c'] M.empty == fromList [(1,'a'),(2,'b'),(3,'c')]
-}
createMapTable ::(Ord a, Ord b) => [a] -> [b] ->  M.Map a b -> M.Map a b
createMapTable [x] [c] m = M.insert x c m
createMapTable (x:xs) (y:ys) m = M.insert x y (createMapTable xs ys m)

{- mineBoard n mines
Places mines as they shall appear in a nxn gameboard according to the indexes in m into an empty M.Map. True represents a mine, false represents no mine
PRE: m =< n**2, no duplicates in mines (m <- mines)
RETURNS: M.Map (a,b) c, where c is True for each (a,b) == getColumnRow n m, where m <- mines
EXAMPLES: mineBoard 16 [1,2,3] == fromList [((1,1),True),((1,2),True),((1,3),True),((1,4),False),((1,5),False),((1,6),False), ... ((16,15),False),((16,16),False)]
          mineBoard 16 [] == fromList [((1,1),False),((1,2),False) ... ((16,15),False),((16,16),False)]
-}
mineBoard n mines = placeMines n [1..end] mines M.empty
    where end = (n*n)

{- placeMines xs mines m
Aux-function to mineBoard
PRE: length mines =< xs, no element in mines is larger (or smaller) than the largest (or smallest) element in xs
RETURNS: M.Map (a,b) c, where c is True for each (a,b) == getColumnRow 16 mi, where mi <- mines
EXAMPLES: placeMines [1,2,3] [3] M.empty == fromList [((1,1),False),((1,2),False),((1,3),True)]
          placeMines [1,2,3] [1,2,3] M.empty == fromList [((1,1),True),((1,2),True),((1,3),True)]
          placeMines [1,2,3] [] M.empty == fromList [((1,1),False),((1,2),False),((1,3),False)]
-}
-- placeMines :: Int -> [Int] -> [Int] ->  M.Map (Int,Int) Mine -> M.Map (Int,Int) Mine
placeMines n [x] [] gameboard = M.insert (getColumnRow n x) False  gameboard
placeMines n [x] _ gameboard = M.insert (getColumnRow n x) True gameboard
placeMines n (x:xs) [] gameboard = M.insert (getColumnRow n x) False (placeMines n xs [] gameboard)
placeMines n (x:xs) (m:ms) gameboard = M.insert (getColumnRow n x) (x==m) (placeMines n xs minesLeft gameboard) 
    where 
        minesLeft = 
            if x == m 
                then ms 
                else (m:ms)

{- getColumnRow n x
Gets the column and row index for x in a nxn gameboard. (Index starts at (1,1))
PRE: 0 < x < n**2 + 1
RETURNS: ((div x n),n) if mod x n == 0, else ((div x n) + 1,mod x n)
EXAMPLES: getColumnRow 16 256 == (16,16)
          getColumnRow 2 1 == (1,1)
-}                
getColumnRow n x = 
            if mod x n == 0
                then ((div x n),n)
                else ((div x n) + 1,mod x n)

{- genMines nm nb
Generates nm random spots for mines in a nbxnb game board
RETURN: a list of n indexes (in range 1 to nb**2) 
EXAMPLES: genMines 40 16 == [7,31,71,88,89,90,95,102,103,104,109,115,129,134,144,146,149,154,155,157,159,163,166,175,188,191,193,195,197,207,209,214,221,226,234,242,246,248,249,254] 
-}
genMines nmines nboard = do
    gen <- newStdGen :: IO StdGen
    let end = (nboard*nboard)
    return $ sort . take nmines . nub $ (randomRs (1,end) gen :: [Int])

{- countSurroundingMines s mb
Counts all mines that surrounds s in  the mineboard mb
PRE: s is a legit column-row index in mb
RETURN: the count of all mines surrounding s
EXAMPLES: ?? This will take a lot of space
------------------------------------
------------------------------------
-}
countSurroundingMines (i,r) mineboard = 
    if M.lookup (i,r) mineboard == Just True
        then Bomb
        else mineType
    where 
        mineCheck (i,r) = if isMine (i,r) == Nothing then 0 else (if isMine (i,r) == Just True then 1 else 0)
        isMine (i,r) = M.lookup (i,r) mineboard
        mineType = mineCount
        num = (mineCheck (i,r-1) + mineCheck (i,r+1) + mineCheck (i-1,r) + mineCheck (i+1,r) + mineCheck (i-1,r-1) + mineCheck (i+1,r-1) + mineCheck (i+1,r+1) + mineCheck (i-1,r+1))
        Just mineCount = M.lookup num $ createMapTable [0..8] [Blank, One, Two, Three, Four, Five, Six, Seven, Eight] M.empty

{- generates game board based on a mineboard by using the Aux-funciton genGameBoard' -}
genGameBoard n mb = genGameBoard' [1..end] mb M.empty n
    where end = n*n

{- genGameBoard' xs mb gb n
Generates a new nxn game board based on a mineboard 
PRE: gb is empty, mb is a legit mine board to xs, xs == [1..n**2]
RETURNS: a game board based on mb
EXAMPLES: ??
-------------------
--------------------
-}
-- genGameBoard' :: [Int] -> M.Map (Int,Int) Mine -> M.Map (Int,Int) (Ansicode, MineData) -> Int -> M.Map (Int,Int) (Ansicode, MineData)
genGameBoard' [x] mineboard gameboard n = M.insert (getColumnRow n x) (white, countSurroundingMines (getColumnRow n x) mineboard)  gameboard 
    where Just white = M.lookup White mineDataAnsi
genGameBoard' (x:xs) mineboard gameboard n = M.insert (getColumnRow n x) (white, countSurroundingMines (getColumnRow n x) mineboard) (genGameBoard' xs mineboard gameboard n) 
    where Just white = M.lookup White mineDataAnsi

{-
This is only an open version of genGameBoard, so that you as a programmer can see that the generated gameboard is correct
This is not a part of the actual game

checkBoard and checkBoard' have the same function specification as genGameBoard, except the mine data is shown
-}
checkBoard mb n = checkBoard' [1..end] mb M.empty n
    where end = (n*n)

checkBoard' [x] mineboard gameboard n = M.insert (getColumnRow n x) (mineDataToAnsi (countSurroundingMines (getColumnRow n x) mineboard), countSurroundingMines (getColumnRow n x) mineboard)  gameboard
checkBoard' (x:xs) mineboard gameboard n = M.insert (getColumnRow n x) (mineDataToAnsi (countSurroundingMines (getColumnRow n x) mineboard), countSurroundingMines (getColumnRow n x) mineboard) (checkBoard' xs mineboard gameboard n) 

{- playerMove gamestate n m 
Plays the game based on the current nxn gamestate with m mines
PRE: gamestate is a legit gamestate
SIDE-EFFECTS: updates the gameboard as the game evolves
-}
playerMove gamestate n m = do
    if victory gamestate n m
        then do
            putStrLn "You won!"
        else do
        putStrLn "Make a move"
        (action,move) <- readMove
        if validMove gamestate move action
            then if action == P
                then do
                    let newgamestate = playMove gamestate move
                    displayGameBoard newgamestate 1 n
                    let Just bomb = M.lookup Bomb  mineDataAnsi
                    if M.lookup move newgamestate == Just (bomb,Bomb)
                        then putStrLn "Sorry, you pressed a mine. Game Over!"
                        else playerMove newgamestate n m
                else do 
                    let newgamestate = flagMove gamestate move
                    displayGameBoard newgamestate 1 n
                    playerMove newgamestate n m
            else do
                putStrLn "Invalid Move."
                playerMove gamestate n m

{- validMove gs m ac
Checks if m is a valid move in gs based on which action ac is
PRE: m is a legit column-row index for gs 
RETURNS: True if m is valid otherwise False
EXAMPLES: ??
--------------------
--------------------
-}
validMove gamestate move action = 
    if action == P 
        then M.lookup move gamestate /= Nothing && displaying == white
        else M.lookup move gamestate /= Nothing && (displaying == white || displaying == flag)
    where 
        Just (displaying, _) = M.lookup move gamestate
        Just white = M.lookup White mineDataAnsi
        Just flag = M.lookup Flag mineDataAnsi

{- readMove
Uses standard input to read an action and a move
RETURNS: an action and a move 
SIDE-EFFECTS: Reads a line (or several) from standard input
-}
readMove = do
  catch (do
    line <- getLine 
    (action,move) <- evaluate (read line)
    return (action,move))  
    ((\_ -> do 
       putStrLn "Invalid input. Correct format: (F/P, (column,row))"
       readMove) :: SomeException -> IO (Action, (Int,Int))) 

{- flagMove gs move
Flag or deflag move in gs
RETURNS: gs with the spot with index move as a flag (or no flag if it previously was flagged)
PRE: move is a legit move in gs
EXAMPLES: ??
--------------
-------------
-}
flagMove gamestate move = 
    if displaying == white
        then M.insert move ((mineDataToAnsi Flag), mineData) (M.delete move gamestate)
        else M.insert move ((mineDataToAnsi White), mineData) (M.delete move gamestate)
    where 
        Just (displaying, mineData) = M.lookup move gamestate
        Just white = M.lookup White mineDataAnsi

{- playMove gs move
Plays move in gs
RETURNS: gs when the move is done
PRE: move is a legit move in gs
EXAMPLES: ??
--------------
-------------
-}
playMove gamestate move =
    if M.lookup move gamestate == Nothing || displaying /= white
        then gamestate
        else if (displaying, mineData) == (white, Blank)
            then playSurroundingSpots (newGameState gamestate move Blank) move
            else newGameState gamestate move mineData
    where 
        Just (displaying, mineData) = M.lookup move gamestate
        newGameState gamestate move mineData = M.insert move ((mineDataToAnsi mineData), mineData) (M.delete move gamestate)
        playSurroundingSpots gamestate (r,i) = playMove (playMove (playMove (playMove (playMove (playMove (playMove (playMove gamestate (r,i-1)) (r,i+1)) (r+1,i+1)) (r-1,i+1)) (r+1,i-1)) (r-1,i-1)) (r-1,i)) (r+1,i) 
        Just white = M.lookup White mineDataAnsi

mineDataToAnsi x = ansi 
    where Just ansi = M.lookup x mineDataAnsi

readGameBoard column row gameboard n = 
    if column < n
        then ansi ++ " | " ++ readGameBoard (column+1) row gameboard n
        else ansi ++ " |    " ++ show row
    where 
        Just (ansi,mine) = M.lookup (column,row) gameboard

displayGameBoard gameboard row n =
    if row <= n
        then do
            putStrLn $ readGameBoard 1 row gameboard n
            displayGameBoard gameboard (row+1) n
        else do 
            putStrLn ""
            putStrLn "1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12| 13| 14| 15| 16 "

    {-
    putStrLn $ readGameBoard 1 1 gameboard
    putStrLn $ readGameBoard 1 2 gameboard
    putStrLn $ readGameBoard 1 3 gameboard
    putStrLn $ readGameBoard 1 4 gameboard
    putStrLn $ readGameBoard 1 5 gameboard
    putStrLn $ readGameBoard 1 6 gameboard
    putStrLn $ readGameBoard 1 7 gameboard
    putStrLn $ readGameBoard 1 8 gameboard
    putStrLn $ readGameBoard 1 9 gameboard
    putStrLn $ readGameBoard 1 10 gameboard
    putStrLn $ readGameBoard 1 11 gameboard
    putStrLn $ readGameBoard 1 12 gameboard
    putStrLn $ readGameBoard 1 13 gameboard
    putStrLn $ readGameBoard 1 14 gameboard
    putStrLn $ readGameBoard 1 15 gameboard
    putStrLn $ readGameBoard 1 16 gameboard
    putStrLn ""
    putStrLn "1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12| 13| 14| 15| 16 "
    -}

{- counts that all no-mine spots have been displayed -}
victory gamestate n m = (sum $ map (checkState gamestate n) [1..end]) == withoutMines
    where 
        end = (n*n)
        withoutMines = (end - m)

{- checkState gs i n
checks if i in gs (a nxn gamestate) is neither a Flag or a White spot 
RETURNS: 1 if i in gs is  neither a Flag or a White spot, otherwis 0
PRE: 0 < i < 257
EXAMPLES: ??
--------------
-----------------

-}
checkState gamestate index n = 
    if displaying /= flag && displaying /= white
        then 1
        else 0
    where
        Just (displaying, _) = M.lookup (getColumnRow n index) gamestate
        Just white = M.lookup White mineDataAnsi
        Just flag = M.lookup Flag mineDataAnsi


{- generates mines, mine board, game board and then plays the game -}
main = do 
    mines <- genMines 40 16
    let mb = mineBoard 16 mines
    let gb = genGameBoard 16 mb

    -- for developement purpose
    let cb = checkBoard mb 16
    displayGameBoard cb 1 16
    --

    displayGameBoard gb 1 16
    playerMove gb 16 40


-- test valid moves
-- write pseudo code for game loop in documentation
-- playMove (initialTestGame()) (Move (....)) -- initialTestGame is just for demonstrationpurpose, 
--      it is a set game board, smaller than 16x16 so that it can be used in funciton specifications

-- 1.hard code
-- 2.illustrate with psudo code or diagram in documentation
-- 3.test code with specific test function
{-}
initialTestGame = 

    where 
        mines = [2,5,10,16]
        mineboard = 
        gameboard = 
-}