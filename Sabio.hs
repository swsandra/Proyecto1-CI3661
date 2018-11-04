module Main 
( main, 
  laberintoEnMem, 
  opcionesDisponibles,
  printMenu,
  instruccionesRuta,
  comenzarLaberintoNuevo,
  construirLaberinto,
  mostrarRecibirOpciones) where
import Laberinto
import Data.Char (digitToInt, isDigit)
import Data.Typeable
import System.IO

-- | Función que crea una instancia de un Laberinto vacío en memoria.
laberintoEnMem :: Laberinto
laberintoEnMem = caminoSinSalida

-- | Función que define las opciones del menú.
opcionesDisponibles :: [(String, String)]
opcionesDisponibles = [
    ("1", "Comenzar a hablar de un laberinto nuevo"),
    ("2", "Pregunar ruta"),
    ("3", "Reportar pared abierta"),
    ("4", "Reportar derrumbe"),
    ("5", "Reportar tesoro tomado"),
    ("6", "Reportar tesoro hallado"),
    ("7", "Dar nombre al laberinto"),
    ("8", "Hablar de un laberinto de nombre conocido")
    ]

-- | Función para mostrar el menú en pantalla.
printMenu :: IO ()
printMenu = putStr $ foldl (\r (x,y) -> r ++ " " ++ x++ ".- " ++ y ++ "\n") "" opcionesDisponibles

-- | Función para mostrar en pantalla las instrucciones de cómo recibir una ruta para un Laberinto.
instruccionesRuta :: IO()
instruccionesRuta = do
    putStrLn "Para indicar una ruta, introduzca una cadena de caracteres."
    putStrLn "Dichos caracteres sólo pueden ser:"    
    putStrLn "  d : girar a la derecha"
    putStrLn "  i : girar a la izquierda"
    putStrLn "  r : seguir recto"
    putStrLn "De introducir un caracter inválido se mostrará un mensaje de error"
    putStrLn "Ejemplo de la ruta derecha-recto-izquierda-derecha: drid"

-- | Función para construir un nuevo Laberinto a partir de una ruta dada por el usuario.
comenzarLaberintoNuevo :: IO()
comenzarLaberintoNuevo = do
    let laberintoEnMem = caminoSinSalida
    putStrLn "Se ha borrado el laberinto en Memoria."
    putStrLn "Indique una ruta para poblar el laberinto."
    instruccionesRuta
    i <- getLine
    construirLaberinto i
    putStrLn "Se ha poblado el laberinto"

-- | Función auxiliar para construir un laberinto.
construirLaberinto (x:xs) = do
    let laberintoEnMem = agregarLaberinto (caminoSinSalida) laberintoEnMem x
    putStr ""

-- | Función para mostrar las opciones y recibir opción del usuario.
mostrarRecibirOpciones :: IO ()
mostrarRecibirOpciones = do
    printMenu
    i:_ <- getLine
    if not ((isDigit i) && ((digitToInt i) `elem` [1..8])) then do
        putStrLn "La opción escogida incorrecta. Introduce una opción válida, viajero. \n"
    else
        case (digitToInt i) of 
            1 -> comenzarLaberintoNuevo
            _ -> putStrLn "Do somth else"
    mostrarRecibirOpciones

-- | Main
main :: IO ()
main = do
    putStrLn "¡Bienvenido, aventurero!"
    putStrLn "Soy el sabio del laberinto. Adelante, escoge una opción"
    mostrarRecibirOpciones