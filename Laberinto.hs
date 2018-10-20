import Data.String

{- Tipo de Datos: Laberinto 
					Trifurcacion: izquierda, recto, derecho -}
data Laberinto = Trifurcacion (Maybe (Laberinto)) (Maybe (Laberinto)) (Maybe (Laberinto))
				| Tesoro String (Maybe (Laberinto))
				deriving (Show)

{-Funciones de Construcción-}

{- Función que retorna un camino sin salida (Trifurcacion
	donde todos los caminos conducen a Nothing)-}
caminoSinSalida :: Laberinto
caminoSinSalida = Trifurcacion (Nothing) (Nothing) (Nothing)

{- Función que recibe un String con la descripción de un tesoro y
	un laberinto y retorna un Tesoro-}
crearTesoro :: String -> Laberinto -> Laberinto
crearTesoro s a = Tesoro s (Just a)

{- Función que recibe una Trifurcacion, un laberinto y
	un indicador de cual camino los relaciona y retorna 
	una Trifurcacion en que se indique que dicho camino
	conduce a dicho laberinto-}
agregarLaberinto :: Laberinto -> Laberinto -> Char -> Laberinto
agregarLaberinto (Trifurcacion izq recto der) lab 'd' = Trifurcacion izq recto (Just lab)
agregarLaberinto (Trifurcacion izq recto der) lab 'i' = Trifurcacion (Just lab) recto der
agregarLaberinto (Trifurcacion izq recto der) lab 'r' = Trifurcacion izq (Just lab) der
agregarLaberinto (Trifurcacion izq recto der) lab _ = error "Se debe especificar dirección del camino ('d', 'i' o 'r')"

--Prueba caminoSinSalida
--let lab1 = Trifurcacion (Just (Trifurcacion Nothing Nothing (Just (caminoSinSalida)))) (Just (caminoSinSalida)) (Just (Trifurcacion Nothing Nothing (Just (caminoSinSalida))))
--Prueba crearTesoro
--let lab2 = Trifurcacion (Just (Trifurcacion Nothing Nothing (Just (caminoSinSalida)))) (Just (crearTesoro "Tesoro1" caminoSinSalida)) (Just (crearTesoro "Tesoro2" caminoSinSalida))
--Prueba agregarLaberinto
--let lab3 = caminoSinSalida
--Agregar los lab1 y lab2
--let lab4 = agregarLaberinto lab3 lab1 'd' ESTO DA ALGO LARGUISIMO
--Pruebas de agregarLaberinto con caminos sin salida
--let lab4 = agregarLaberinto lab3 caminoSinSalida 'd'
--Pruebalo con el primer parametro como un laberinto mas complejo (que si Trifurcacion Nothing Nothing (Just(caminoSinSalida)))

{-Funciones de Acceso-}
