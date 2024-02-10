-- Función para calcular la media de una lista de números
calcularMedia :: [Float] -> Float
calcularMedia numeros = sum numeros / fromIntegral (length numeros)

-- Función para calcular la desviación estándar de una lista de números
calcularDesviacionEstandar :: [Float] -> Float
calcularDesviacionEstandar numeros =
  let mediaNumeros = calcularMedia numeros
      cantidadNumeros = fromIntegral (length numeros)
      sumaDiferenciasCuadradas = sum $ map (\numero -> (numero - mediaNumeros) ^ 2) numeros
  in sqrt (sumaDiferenciasCuadradas / cantidadNumeros)

-- Función para calcular la puntuación típica de un valor en una muestra
calcularPuntuacionTipica :: Float -> [Float] -> Float
calcularPuntuacionTipica valor numeros = (valor - calcularMedia numeros) / calcularDesviacionEstandar numeros

-- Función para encontrar los valores atípicos en una muestra
encontrarValoresAtipicos :: [Float] -> [Float]
encontrarValoresAtipicos numeros = filter (\numero -> abs (calcularPuntuacionTipica numero numeros) > 3) numeros

-- Imprimir los valores atípicos
imprimirValoresAtipicos :: [Float] -> IO ()
imprimirValoresAtipicos [] = putStrLn "No hay valores atípicos."
imprimirValoresAtipicos atipicos = do
    putStrLn "Valores atípicos:"
    mapM_ print atipicos

-- Ejemplo de uso
main :: IO ()
main = do
    let muestraNumeros = [10.0, 20.0, 15.0, 12.0, 100.0, 25.0, 30.0, 8.0, 9.0, 11.0, 500.0, -200.0]
    let valoresAtipicos = encontrarValoresAtipicos muestraNumeros
    imprimirValoresAtipicos valoresAtipicos
