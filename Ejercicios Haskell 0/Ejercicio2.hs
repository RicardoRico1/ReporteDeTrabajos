calcularFactorial :: Int -> Int
calcularFactorial 0 = 1
calcularFactorial m = m * calcularFactorial (m - 1)

ejecutaFactorial :: IO ()
ejecutaFactorial = do
    putStrLn "Escribe un número para su factorial:"
    valorStr <- getLine
    let valor = read valorStr :: Int
    putStrLn $ "Factorial de " ++ show valor ++ " es: " ++ show (calcularFactorial valor)
