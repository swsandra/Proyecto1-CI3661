module Main (main) where
import Laberinto
import Data.Char (digitToInt, isDigit)
import Data.Typeable

-- Crear laberinto vacio en memoria
laberintoEnMem :: Laberinto
laberintoEnMem = caminoSinSalida

-- Definicion opciones de menu
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

-- Funcion para imprimir menu
printMenu :: IO ()
printMenu = putStr $ foldl (\r (x,y) -> r ++ " " ++ x++ ".- " ++ y ++ "\n") "" opcionesDisponibles

instruccionesRuta :: IO()
instruccionesRuta = do
    putStrLn "Para indicar una ruta, introduzca una cadena de caracteres."
    putStrLn "Dichos caracteres sólo pueden ser:"    
    putStrLn "  d : girar a la derecha"
    putStrLn "  i : girar a la izquierda"
    putStrLn "  r : seguir recto"
    putStrLn "De introducir un caracter invalido se mostrará un mensaje de error"

comenzarLaberintoNuevo :: IO Laberinto
comenzarLaberintoNuevo = do
    let laberintoEnMem = caminoSinSalida
    putStrLn "Se ha borrado el laberinto en Memoria."
    putStrLn "Indique una ruta para poblar el laberinto."
    instruccionesRuta
    i <- getLine
    let laberintoEnMem = construirLaberinto laberintoEnMem i
    return laberintoEnMem

construirLaberinto :: Laberinto -> [Char] -> Laberinto
construirLaberinto lab str = if ((length str) == 1) then
                                agregarLaberinto (caminoSinSalida) lab (head str)
                             else
                                agregarLaberinto (caminoSinSalida) (construirLaberinto lab (tail str)) (head str)


-- Funcion para mostrar las opciones y recibir del user
mostrarRecibirOpciones :: IO ()
mostrarRecibirOpciones = do
    printMenu
    i:_ <- getLine
    if not ((isDigit i) && ((digitToInt i) `elem` [1..8])) then do
        putStrLn "La opción escogida incorrecta. Introduce una opción válida, viajero. \n"
    else
        case (digitToInt i) of 
            1 -> do
                    let laberintoEnMem = comenzarLaberintoNuevo
                    putStrLn ""
            --2 -> do
                    --let puntoActual = 
            _ -> putStrLn "Do somth else"
    mostrarRecibirOpciones

main :: IO ()
main = do
    putStrLn "¡Bienvenido, aventurero!"
    putStrLn "Soy el sabio del laberinto. Adelante, escoge una opción"
    mostrarRecibirOpciones