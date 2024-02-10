import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as Map

-- Función para asignar calificaciones textuales a las notas numéricas
calificarNota :: Int -> String
calificarNota nota
    | nota >= 95 = "Excelente"
    | nota >= 85 = "Notable"
    | nota >= 75 = "Bueno"
    | nota >= 70 = "Suficiente"
    | otherwise = "Desempeño Insuficiente"

-- Función para convertir las asignaturas a mayúsculas y asignar calificaciones textuales
calificarAsignaturas :: Map String Int -> Map String String
calificarAsignaturas calificaciones =
    Map.mapKeys (map toUpper) $ Map.map calificarNota calificaciones

-- Ejemplo de uso
main :: IO ()
main = do
    let notasAlumno = Map.fromList [("Matemáticas", 90), ("Historia", 80), ("Ciencias", 65)]
    let calificacionesFinales = calificarAsignaturas notasAlumno
    print calificacionesFinales
