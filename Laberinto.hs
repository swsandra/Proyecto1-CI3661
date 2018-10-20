import Data.String

{- Tipo de Datos: Laberinto 
					Trifurcacion: derecha, izquierda, recto -}
data Laberinto = Trifurcacion (Maybe (Laberinto)) (Maybe (Laberinto)) (Maybe (Laberinto))
				| Tesoro String (Maybe (Laberinto))
				deriving (Show)

{- Función que retorna un camino sin salida (Trifurcacion
	donde todos los caminos conducen a Nothing)-}
caminoSinSalida :: Maybe(Laberinto)
caminoSinSalida = Just(Trifurcacion (Nothing) (Nothing) (Nothing))

{- Función que recibe un String con la descripción de un tesoro y
	un laberinto y retorna un Tesoro-}
crearTesoro :: String -> Maybe(Laberinto) -> Maybe(Laberinto)
crearTesoro s a = Just(Tesoro s a)

{- Función que recibe una Trifurcacion, un laberinto y
	un indicador de cual camino los relaciona y retorna 
	una Trifurcacion en que se indique que dicho camino
	conduce a dicho laberinto-}
agregarLaberinto :: Maybe(Laberinto) -> Maybe(Laberinto) -> Char -> Maybe(Laberinto)
agregarLaberinto trif lab _ = error "Se debe especificar dirección del camino ('d', 'i' o 'r')"
agregarLaberinto trif lab 'd' = trif (lab) (_) (_)
agregarLaberinto trif lab 'i' = trif (_) (lab) (_)
agregarLaberinto trif lab 'r' = trif (_) (_) (lab)
