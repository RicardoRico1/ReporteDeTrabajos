-- Definir la función para incrementar un valor
incrementarValor :: Float -> Float
incrementarValor valor = valor + 1.0 

-- Definir la función para incrementar cada elemento de una lista
incrementarLista :: (Float -> Float) -> [Float] -> [Float]
incrementarLista _ [] = []  
incrementarLista funcionIncremento (valorActual:valoresRestantes) = funcionIncremento valorActual : incrementarLista funcionIncremento valoresRestantes 

-- Lista de valores
valores :: [Float]
valores = [1.0, 10.0, 2.0, 20.0, 3.0, 30.0, 4.0, 40.0, 5.0, 50.0, 6.0, 60.0]

-- Aplicar incremento a los valores de la lista
valoresIncrementados :: [Float]
valoresIncrementados = incrementarLista incrementarValor valores

-- Imprimir el resultado
main :: IO ()
main = do
    print valoresIncrementados
