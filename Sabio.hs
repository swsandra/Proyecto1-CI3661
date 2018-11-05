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

preguntarRuta :: Laberinto -> IO ()
preguntarRuta lab = do
    putStrLn "Introduzca la ruta a seguir en el laberinto."
    instruccionesRuta
    i <- getLine
    let laberintoEnMem = recorrerLaberinto lab i
    if (evaluarLaberinto laberintoEnMem == 'S') then do
        putStrLn "Se ha llegado a un camino sin salida."
        putStrLn "GAME OVER."
    else if (evaluarLaberinto laberintoEnMem == 'E') then do
        putStrLn "¡Has encontrado un TESORO!"
        putStrLn "FIN"
    else do
        putStrLn "No ha llegado a nada. Puede continuar o empezar desde el inicio."
        putStrLn "1.- Continuar"
        putStrLn "2.- Volver al inicio"
        i:_ <- getLine
        if not ((isDigit i) && ((digitToInt i) `elem` [1..2])) then do
            putStrLn "La opción escogida es incorrecta."
            putStrLn "Por errores del viajero, se llevara al principio del programa. \n"
            mostrarRecibirOpciones lab
        else
            case (digitToInt i) of
                1 -> preguntarRuta laberintoEnMem
                2 -> preguntarRuta lab

repParedAbiertaMenu :: Laberinto -> IO ()
repParedAbiertaMenu lab = do
    putStrLn "Introduzca una ruta para abrir la pared, de ser posible"
    instruccionesRuta
    i <- getLine
    let laberintoEnMem = reportarParedAbierta lab i
    putStrLn "Se ha concluido la operación.\n"
    mostrarRecibirOpciones laberintoEnMem

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

escribirEnArchivo :: Laberinto -> IO ()
escribirEnArchivo lab = do
    putStrLn "Introduzca el path del archivo donde escribir el laberinto actual"
    i <- getLine
    escribirLaberinto i lab
    putStrLn "Se ha exportado el laberinto."
    mostrarRecibirOpciones lab

cargarLaberinto lab = do
    putStrLn "Indique el path del archivo que contiene el laberinto."
    i <- getLine
    --let laberintoEnMem = leerLaberinto i
    putStrLn "Se ha importado el laberinto al sistema."
    --mostrarRecibirOpciones laberintoEnMem

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
            2 -> preguntarRuta laberintoEnMem
            3 -> repParedAbiertaMenu laberintoEnMem
            4 -> repDerrumbeMenu laberintoEnMem
            --5 -> tesorotomado
            --6 -> tesorohallado
            7 -> escribirEnArchivo laberintoEnMem
            --8 -> cargarLaberinto      DESCOMENTA ESTO!!!
    mostrarRecibirOpciones laberintoEnMem

main :: IO ()
main = do
    putStrLn "¡Bienvenido, aventurero!"
    putStrLn "Soy el sabio del laberinto. Adelante, escoge una opción"
    mostrarRecibirOpciones caminoSinSalida