module Main (main) where
import Laberinto
import Data.Char (digitToInt, isDigit)
import Data.Typeable
import System.IO

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
    putStrLn "De introducir un caracter invalido se ignorará el mismo"
    putStrLn "Ejemplo de la ruta derecha-recto-izquierda-derecha: drid"

comenzarLaberintoNuevo :: IO ()
comenzarLaberintoNuevo = do
    putStrLn "Se ha borrado el laberinto en Memoria."
    putStrLn "Indique una ruta para poblar el laberinto."
    instruccionesRuta
    i <- getLine
    let laberintoEnMem = construirLaberinto caminoSinSalida i
    putStrLn "Se ha cargado el laberinto. \n"
    mostrarRecibirOpciones laberintoEnMem

construirLaberinto :: Laberinto -> [Char] -> Laberinto
construirLaberinto lab str = if ((length str) == 1) then
                                agregarLaberinto (caminoSinSalida) lab (head str)
                             else
                                agregarLaberinto (caminoSinSalida) (construirLaberinto lab (tail str)) (head str)

repParedAbiertaMenu :: Laberinto -> IO ()
repParedAbiertaMenu lab = do
    putStrLn "Introduzca una ruta para abrir la pared, de ser posible"
    instruccionesRuta
    i <- getLine
    let laberintoEnMem = reportarParedAbierta lab i
    putStrLn "Se ha concluido la operación.\n"
    mostrarRecibirOpciones laberintoEnMem

reportarParedAbierta :: Laberinto -> [Char] -> Laberinto
reportarParedAbierta lab [] = lab
reportarParedAbierta lab str =
                        if (recorrerLaberinto lab ([head str]) == lab) then
                            if ((length str) == 1) then
                                agregarLaberinto (lab) caminoSinSalida (head str)
                            else
                                agregarLaberinto (lab) (construirLaberinto caminoSinSalida (tail str)) (head str)
                        else
                            reportarParedAbierta (recorrerLaberinto lab ([head str])) (tail str)

repDerrumbeMenu :: Laberinto -> IO ()
repDerrumbeMenu lab = do
    putStrLn "Introduzca una ruta para derrumbar una pared, de ser posible."
    instruccionesRuta
    i <- getLine
    putStrLn "Introduzca la dirección de la pared que desea derrumbar."
    putStrLn "Los caracteres permitidos sólo pueden ser los indicados anteriormente."
    m <- getLine
    if ((length m) > 1 || (m/="d" && m/="i" && m/="d")) then do
        putStrLn "El caracter introductido no es valido o es una cadena de caracteres."
        putStrLn "Regresando al menu."
        mostrarRecibirOpciones lab
    else do
        let laberintoEnMem = reportarDerrumbe lab i m
        putStrLn "Se ha concluido la operación.\n"
        mostrarRecibirOpciones laberintoEnMem

reportarDerrumbe :: Laberinto -> [Char] -> [Char] -> Laberinto
reportarDerrumbe (Trifurcacion izq rect der) [] [] = Trifurcacion izq rect der
reportarDerrumbe (Trifurcacion izq rect der) str [] = Trifurcacion izq rect der
reportarDerrumbe (Trifurcacion izq rect der) [] char = 
        case (head char) of
            'i' -> Trifurcacion (Nothing) rect der
            'r' -> Trifurcacion izq (Nothing) der
            'd' -> Trifurcacion izq rect (Nothing)
reportarDerrumbe (Trifurcacion izq rect der) str char=
        reportarDerrumbe (recorrerLaberinto (Trifurcacion izq rect der) str) [] char

-- Funcion para mostrar las opciones y recibir del user
mostrarRecibirOpciones :: Laberinto -> IO ()
mostrarRecibirOpciones laberintoEnMem = do
    printMenu
    i:_ <- getLine
    if not ((isDigit i) && ((digitToInt i) `elem` [1..8])) then do
        putStrLn "La opción escogida incorrecta. Introduce una opción válida, viajero. \n"
    else
        case (digitToInt i) of 
            1 -> comenzarLaberintoNuevo
            --2 -> do
                    --let puntoActual = 
            3 -> repParedAbiertaMenu laberintoEnMem
            4 -> repDerrumbeMenu laberintoEnMem
            _ -> putStrLn "Do somth else"
    mostrarRecibirOpciones laberintoEnMem

main :: IO ()
main = do
    putStrLn "¡Bienvenido, aventurero!"
    putStrLn "Soy el sabio del laberinto. Adelante, escoge una opción"
    mostrarRecibirOpciones caminoSinSalida