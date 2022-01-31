removeNonUppercase :: String -> String 
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]  

main = do
    print(removeNonUppercase "HeLLo")