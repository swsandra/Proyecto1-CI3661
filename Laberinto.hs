import Data.String

{- Tipo de Datos: Laberinto 
					Trifurcacion: derecha, izquierda, recto -}
data Laberinto a = Trifurcacion (Maybe (Laberinto a)) (Maybe (Laberinto a)) (Maybe (Laberinto a))

data Tesoro t = String t (Maybe (Laberinto t))

