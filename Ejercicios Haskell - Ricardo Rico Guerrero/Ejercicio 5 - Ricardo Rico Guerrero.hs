-- Función para calcular la suma de los cuadrados de los elementos de una lista
sumaCuadrados :: [Float] -> Float
sumaCuadrados [] = 0
sumaCuadrados (n:ns) = n * n + sumaCuadrados ns

-- Función para calcular el módulo de una lista interpretada como vector
moduloLista :: [Float] -> Float
moduloLista lista = sqrt (sumaCuadrados lista)

-- Ejemplo de uso
main :: IO ()
main = do
    let listaNumeros = [3.0, 4.0] -- Lista que representa el vector [3, 4]
    let resultadoModulo = moduloLista listaNumeros
    print resultadoModulo

