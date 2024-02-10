-- Definir la función de descuento que toma dos valores
calcularDescuento :: Float -> Float -> Float
calcularDescuento precioOriginal porcentajeDescuento = precioOriginal - (precioOriginal * porcentajeDescuento / 100)

-- Definir la función para aplicar descuento a una lista de precios
aplicarDescuentos :: [Float] -> (Float -> Float -> Float) -> [Float]
aplicarDescuentos [] _ = []  
aplicarDescuentos (precio:descuento:restoPrecios) funcionDescuento = funcionDescuento precio descuento : aplicarDescuentos restoPrecios funcionDescuento

-- Lista de precios y descuentos
listaPreciosDescuentos :: [Float]
listaPreciosDescuentos = [1.0, 10.0, 2.0, 20.0, 3.0, 30.0, 4.0, 40.0, 5.0, 50.0, 6.0, 60.0]

-- Aplicar descuento a los precios
preciosConDescuento :: [Float]
preciosConDescuento = aplicarDescuentos listaPreciosDescuentos calcularDescuento

-- Imprimir el resultado
main :: IO ()
main = do
    print preciosConDescuento

