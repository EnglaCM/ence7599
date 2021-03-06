
import Test.HUnit
import Data.Colour.Names
import System.Console.ANSI
import Data.Char

t = 2
c = '●'

main = do 
    print(t)
    putStrLn $ "\ESC[0mdefault"
    putStrLn $ "\ESC[30mblack"
    putStrLn $ "\ESC[0mdefault"
    putStrLn $ "\ESC[30mblack"
    putStrLn $ "\ESC[31mred"
    putStrLn $ "\ESC[32mgreen"
    putStrLn $ "\ESC[33myellow"
    putStrLn $ "\ESC[34mblue"
    putStrLn $ "\ESC[35mmagenta"
    putStrLn $ "\ESC[36mcyan"
    putStrLn $ "\ESC[37mwhite"
    putStrLn $ "\ESC[90mblack"
    putStrLn $ "\ESC[91mred"
    putStrLn $ "\ESC[92mgreen"
    putStrLn $ "\ESC[93myellow"
    putStrLn $ "\ESC[94mblue"
    putStrLn $ "\ESC[95mmagenta"
    putStrLn $ "\ESC[96mcyan"
    putStrLn $ "\ESC[96mO"
    putStrLn $ "\ESC[96;1;4mHello\ESC[0m"
    putStrLn $ "\ESC[96;1mO"
    putStrLn $ "\ESC[91;1mO"
    putStrLn $ "\ESC[92;1mO"
    putStrLn $ "\ESC[93;1mO"
    putStrLn $ "\ESC[94;1mO"
    putStrLn $ "\ESC[95;1mO"
    putStrLn $ "\ESC[37;1mO"
    putStrLn $ "\ESC[90;1mO"
    putStrLn $ "\ESC[31;1mO"
    putStrLn $ "\ESC[32;1mO"
    putStrLn $ "\ESC[33;1mO"
    putStrLn $ "\ESC[34;1mO"
    putStrLn $ "\ESC[35;1mO"
    putStrLn $ "\ESC[36;1mO"
    putStrLn $ "\ESC[37;1mO"
    putStrLn $ "\ESC[90;1mO"
    -- putStrLn $ "\ESC[96;1;48;5;96m \ESC[0m"
    putStrLn $ "\ESC[97mwhite\ESC[0m"
    putStrLn $ show "\9679"

{- 
\ESC[0m - resets it to normal
\ESC[96 - sets colour to cyan
\ESC[96;1 - sets colour to cyan and makes text bold

-}