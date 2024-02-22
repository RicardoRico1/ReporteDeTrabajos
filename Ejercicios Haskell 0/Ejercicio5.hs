invertirLista :: [a] -> [a]
invertirLista [] = []
invertirLista (y:ys) = invertirLista ys ++ [y]

ejecutaInversion :: IO ()
ejecutaInversion = do
    putStrLn "Escribe elementos de la lista separados por espacio:"
    entrada <- getLine
    let elementos = map read (words entrada) :: [Int]
    putStrLn $ "Lista al revés: " ++ show (invertirLista elementos)
