foobar :: IO Int 
foobar = do
    putStrLn "A" -- output
    x <- putStrLn "B"
    let y = putStrLn "C"
    return x  -- output      return x :: Monad m => m ()
    y -- output
    putStrLn "D" -- output
    return 42 -- result

foo = do
    line <- getLine 
    return $ bar line
    return $ bar line
    return $ bar line
    return $ bar line 
    return 32
    return 42
    return $ sum [1,2,3]
    return 31 -- returns only the last one

bar l = l == "3060"

barfoo = do
    x <- putStrLn "A"
    return x -- will putStrLn "A"
    return 32 -- will not return this
    putStrLn "B" -- wil putStrLn "B"
    return 33 -- will return this