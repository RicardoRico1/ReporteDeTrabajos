calculaSuma :: [Int] -> Int
calculaSuma []     = 0
calculaSuma (y:ys) = y + calculaSuma ys

ejecutaSuma :: IO ()
ejecutaSuma = do
    let numeros = [1, 2, 3, 4, 5]
    putStrLn $ "Suma total: " ++ show (calculaSuma numeros)
