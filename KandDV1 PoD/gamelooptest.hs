import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do 
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "Guess a number between 1 an 10. You have 3 guesses"
    guess randNumber 3
    askForNumber newGen 

guess :: Int -> Int -> IO ()
guess num 0 = putStrLn $ "Sorry, out of guesses! It was " ++ show num
guess num n = do
    numberString <- getLine
    when (not $ null numberString) $ do
    let number = read numberString
    if num == number
        then putStrLn "You are correct!"
    else (do
        putStrLn $ "Incorrect! You have " ++ (show (n-1)) ++ " more guesses"
        guess num (n-1))