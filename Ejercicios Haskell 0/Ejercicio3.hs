listaPares :: Int -> [Int]
listaPares n = [z | z <- [0..n], even z]

ejecutaPares :: IO ()
ejecutaPares = do
    putStrLn "Define un límite:"
    limiteStr <- getLine
    let limite = read limiteStr :: Int
    putStrLn $ "Pares hasta " ++ show limite ++ ": " ++ show (listaPares limite)
