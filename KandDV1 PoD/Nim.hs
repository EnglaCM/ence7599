import Control.Exception
import Prelude hiding(catch)
import System.Random

{- Move
  Represents a move. The first element of the tuple
    is the pile to move from, the second element is the amount to move.
  Invariant: The first element refers to a valid pile. The second >= 0
-}
type Move = (Int, Int) 


{- Game State
  Represents the number of stones in the three piles.
  Invariant: each element of the tuple >= 0.
-}
type GameState = (Int, Int, Int)


type Player = String

{- readMove
   Reads a move from standard input
   Returns: A move object 
   Side-effects: Reads one or more lines from standard input 
-}
readMove :: GameState -> IO Move
readMove gameState = do
  catch (do
    line <- getLine 
    move <- evaluate (read line) -- evaluate required to force conversion of line to Move
    if validMove gameState move 
       then 
          return move
       else do 
          putStrLn "Invalid input. Correct num: 0 < num < 4, Correct pile: 0 < pile < 4"
          readMove gameState)  
    ((\_ -> do   -- exception handler (hittas ett fel i catch går det vidare till denna funktion, en lambda funktion för att vi inte tar något argument)
       putStrLn "Invalid input. Correct format: (pileNumber,amount)"
       readMove gameState) :: SomeException -> IO Move)


{- validMove gs m
   Determines whether a move is valid in a given game state
   Returns: True if and only if the move refers to a pile and the number 
    of elements to remove is greater than 0 and less than number of
    elements in the pile
-}
validMove :: GameState -> Move -> Bool
validMove (a, _, _) (1, n) = a >= n && n > 0 && n < 4
validMove (_, b, _) (2, n) = b >= n && n > 0 && n < 4 
validMove (_, _, c) (3, n) = c >= n && n > 0 && n < 4
validMove _ _ = False


{- victory gs
   Determines whether a game state is the winning position.
   Returns: True when all stone piles are empty
-}
victory :: GameState -> Bool
victory (0, 0, 0) = True
victory _ = False

{- playmove state move 
   Updates the game state after making a move
   Pre: validMove state move
   Returns: A valid game state
-}
playMove :: GameState -> Move -> GameState
playMove (a, b, c) (1, n) = (a - n, b, c)
playMove (a, b, c) (2, n) = (a, b - n, c)
playMove (a, b, c) (3, n) = (a, b, c - n)


{- gameState
   Generates a fresh game state
   Returns: A valid game state
   Side effect: None (at present)
-}
genGameState :: Int -> IO GameState
genGameState upperBound = do
   gen <- getStdGen
   let (rNum1, nNum1) = randomR (1,upperBound) gen :: (Int, StdGen)
   let (rNum2, nNum2) = randomR (1,upperBound) nNum1 :: (Int, StdGen)
   let (rNum3, _) = randomR (1,upperBound) nNum2 :: (Int, StdGen)
   return (rNum1,rNum2,rNum3)


{- printGameState gs
   Print a game state
   Side-effect: Displays game state to standard output
-}
printGameState :: GameState -> IO ()
printGameState (a, b, c) = do
  putStrLn $ "Pile 1 contains " ++ (show a) ++ " stones."
  putStrLn $ "Pile 2 contains " ++ (show b) ++ " stones."
  putStrLn $ "Pile 3 contains " ++ (show c) ++ " stones."


{- printMove player move
   Print a move for a given player
   Side-effect: Displays move on standard output
-}
printMove :: Player -> Move -> IO ()
printMove player (pile, amount) = putStrLn $ player ++ " removes " ++ (show amount) ++ " stones from pile " ++ (show pile)


{- main
   Run the game
   Side-effects: Quite a lot, actually
-}
main :: IO ()
main = do 
  putStrLn ""
  putStrLn "Welcome to Nim."
  putStrLn "What do you want as an upper bound?"
  upperBound <- getLine
  gameState <- genGameState $ read upperBound
  play gameState

{- play gs
   Play the game
   Pre: gs is valid and not the victory state
   Side-effect: The game interaction -- it never returns
-}
play gameState = do
  printGameState gameState
  newGameState <- playerMove gameState
  if victory newGameState then do
    putStrLn "Player won!"
    putStrLn "Do you want to play again (y/n)?"
    again <- getLine 
    quitOption again
   else do
    newNewGameState <- computerMove newGameState  
    if victory newNewGameState then do
      putStrLn "Computer won!"
      putStrLn "Do you want to play again (y/n)?"
      again <- getLine 
      quitOption again
     else
      play newNewGameState      

{- quitOption again
   Continue to play the game if again /= "n", otherwise it quits the game
   Pre: gs is valid and not the victory state
   Returns: a new game or "Good bye!"
   Side-effect: Reruns the game or finishes the game
-}
quitOption :: String -> IO ()
quitOption again = do
   if again == "n" 
      then 
         putStrLn "Good bye!"
      else
         main

{- playerMove gs
   Perform the player's move
   Pre: gs is valid and not the victory state
   Returns: a new game state
   Side-effect: Displays a description of the players's move
-}
playerMove :: GameState -> IO GameState
playerMove gameState = do
  putStrLn "Your move."
  move <- readMove gameState
  printMove "Player" move
  if validMove gameState move then 
    return $ playMove gameState move
   else do
    putStrLn "Invalid Move."
    playerMove gameState

{- computerMove gs
   Perform the computer's move
   Pre: gs is valid and not the victory state
   Returns: a new game state
   Side-effect: Displays a description of the computer's move
-} 
computerMove :: GameState -> IO GameState
computerMove gameState = do 
  let move = calculateComputerMove gameState
  printMove "Computer" move
  return $ playMove gameState move
  
  
{- calculateComputerMove gs
   Calculate the (best) next move for the computer
   Returns: A valid move for gs
-}
calculateComputerMove :: GameState -> Move
calculateComputerMove (a,b,c)
                              | a > 0 && a < 4    = (1, a)
                              | a > 0             = (1, 3)
                              | b > 0 && b < 4    = (2, b)
                              | b > 0             = (2, 3)
                              | c > 0 && c < 4    = (3, c)
                              | otherwise = (3, 3)
       
