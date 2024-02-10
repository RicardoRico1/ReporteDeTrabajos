import Data.Map (Map)
import qualified Data.Map as Map

-- Función para dividir una frase en palabras
dividirEnPalabras :: String -> [String]
dividirEnPalabras = words

-- Función para calcular la longitud de una palabra
longitudPalabra :: String -> Int
longitudPalabra = length

-- Función para crear un mapa de palabras y sus longitudes
crearMapaPalabras :: String -> Map String Int
crearMapaPalabras frase =
  let palabras = dividirEnPalabras frase
      longitudesPalabras = map longitudPalabra palabras
   in Map.fromList (zip palabras longitudesPalabras)

-- Ejemplo de uso
main :: IO ()
main = do
  let frase = "Hola en este momento son las 10:50 pm y ya me canse"
  let mapaPalabras = crearMapaPalabras frase
  print mapaPalabras

