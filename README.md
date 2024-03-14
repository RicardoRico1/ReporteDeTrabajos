esNumeroPrimo :: Int -> Bool
esNumeroPrimo num
  | num < 2 = False
  | otherwise = all (\divisor -> num `mod` divisor /= 0) [2..limite]
  where limite = floor . sqrt $ fromIntegral num


convertirNumeroATexto :: Int -> String
convertirNumeroATexto 0 = "cero"
convertirNumeroATexto 1 = "uno"
convertirNumeroATexto num | num == 1000000 = "un millón"
convertirNumeroATexto num = prefijoMiles ++ textoMenorDeMil sobrante
  where
    (cantidadMiles, sobrante) = num `divMod` 1000
    prefijoMiles
      | cantidadMiles > 1 = textoMenorDeMil cantidadMiles ++ " mil "
      | cantidadMiles == 1 = "mil "
      | otherwise = ""


textoMenorDeMil :: Int -> String
textoMenorDeMil cantidad
  | cantidad == 0 = ""
  | otherwise = textoCentenas ++ textoDecenas
  where
    (centenas, restoCentenas) = cantidad `divMod` 100
    (decenas, unidades) = restoCentenas `divMod` 10
    textoCentenas
      | centenas == 1 && restoCentenas == 0 = "cien "
      | centenas >= 1 = listaCentenas !! centenas
      | otherwise = ""
    textoDecenas
      | decenas == 2 && unidades > 0 = "veinti" ++ listaUnidades !! unidades
      | decenas == 1 = listaEspeciales !! unidades
      | decenas >= 2 = if unidades == 0 then listaDecenas !! decenas else listaDecenas !! decenas ++ "y " ++ listaUnidades !! unidades
      | unidades > 0 = listaUnidades !! unidades
      | otherwise = ""
    listaCentenas = ["", "ciento ", "doscientos ", "trescientos ", "cuatrocientos ", "quinientos ", "seiscientos ", "setecientos ", "ochocientos ", "novecientos "]
    listaDecenas = ["", "", "veinte ", "treinta ", "cuarenta ", "cincuenta ", "sesenta ", "setenta ", "ochenta ", "noventa "]
    listaUnidades = ["", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve"]
    listaEspeciales = ["diez", "once", "doce", "trece", "catorce", "quince", "dieciséis", "diecisiete", "dieciocho", "diecinueve"]


fizzBuzzPrimo :: Int -> String
fizzBuzzPrimo n
  | esNumeroPrimo n = "FizzBuzz!"
  | otherwise = convertirNumeroATexto n

main :: IO ()
main = do
  putStrLn "Introduce un número entre 0 y 1000000:"
  entrada <- getLine
  let numero = read entrada :: Int
  putStrLn $ fizzBuzzPrimo numero

