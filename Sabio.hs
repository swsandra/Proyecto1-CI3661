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
laberintoEnMem :: Laberinto -- ^ Valor de retorno: Laberinto vacío.
laberintoEnMem = caminoSinSalida

-- | Función que define las opciones del menú.
opcionesDisponibles :: [(String, String)] -- ^ Valor de retorno: Diccionario de número de opción y descripción de la opción.
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
printMenu :: IO () -- ^ Valor de retorno: IO.
printMenu = putStr $ foldl (\r (x,y) -> r ++ " " ++ x++ ".- " ++ y ++ "\n") "" opcionesDisponibles

-- | Función para mostrar en pantalla las instrucciones de cómo recibir una ruta para un Laberinto.
instruccionesRuta :: IO() -- ^ Valor de retorno: IO.
instruccionesRuta = do
    putStrLn "Para indicar una ruta, introduzca una cadena de caracteres."
    putStrLn "Dichos caracteres sólo pueden ser:"    
    putStrLn "  d : girar a la derecha"
    putStrLn "  i : girar a la izquierda"
    putStrLn "  r : seguir recto"
    putStrLn "De introducir un caracter inválido se mostrará un mensaje de error"
    putStrLn "Ejemplo de la ruta derecha-recto-izquierda-derecha: drid"

-- | Función para construir un nuevo Laberinto a partir de una ruta dada por el usuario.
comenzarLaberintoNuevo :: IO() -- ^ Valor de retorno: IO.
comenzarLaberintoNuevo = do
    putStrLn "Se ha borrado el laberinto en Memoria."
    putStrLn "Indique una ruta para poblar el laberinto."
    instruccionesRuta
    i <- getLine
    let laberintoEnMem = construirLaberinto caminoSinSalida i
    putStrLn "Se ha cargado el laberinto. \n"
    mostrarRecibirOpciones laberintoEnMem

-- | Función que pregunta la ruta a recorrer en un Laberinto dado.
preguntarRuta :: Laberinto -- ^ Laberinto que se quiere recorrer.
              -> IO () -- ^ Valor de retorno: IO.
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
{- | 
    Función que imprime las instrucciones para reportar una pared abierta.
    Recibe el laberinto en memoria, y con la ruta que pide al usuario
    se llama a la función correspondiente.
-}
repParedAbiertaMenu :: Laberinto -- ^ 
                    -> IO () -- ^ Valor de retorno: IO.
repParedAbiertaMenu lab = do
    putStrLn "Introduzca una ruta para abrir la pared, de ser posible"
    instruccionesRuta
    i <- getLine
    let laberintoEnMem = reportarParedAbierta lab i
    putStrLn "Se ha concluido la operación.\n"
    mostrarRecibirOpciones laberintoEnMem

{- |
    Función que imprime las instrucciones para reportar una pared abierta.
    Recibe el laberinto en memoria, y con una ruta y dirección que pide al
    usuario se llama a la función correspondiente.
-}
repDerrumbeMenu :: Laberinto -- ^ .
                -> IO () -- ^ Valor de retorno: IO.
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


-- | Función que escribe un Laberinto en un archivo.
escribirEnArchivo :: Laberinto -- ^ Laberinto que se desea escribir en un archivo.
                  -> IO () -- ^ Valor de retorno: IO.
escribirEnArchivo lab = do
    putStrLn "Introduzca el path del archivo donde escribir el laberinto actual"
    i <- getLine
    escribirLaberinto i lab
    putStrLn "Se ha exportado el laberinto."
    mostrarRecibirOpciones lab

-- | Función para mostrar las opciones y recibir las introducidas por el usuario.
mostrarRecibirOpciones :: Laberinto -- ^ Laberinto que se tiene en memoria.
                       -> IO () -- ^ Valor de retorno: IO.
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
            8 -> do
                putStrLn "Indique el path del archivo que contiene el laberinto."
                i <- getLine
                x <- readFile i
                let lab = read x :: Laberinto
                putStrLn "Se ha importado el laberinto al sistema."
                putStrLn $ show lab
                mostrarRecibirOpciones lab
    mostrarRecibirOpciones laberintoEnMem

-- | Main
main :: IO () -- ^ Valor de retorno: IO.
main = do
    putStrLn "¡Bienvenido, aventurero!"
    putStrLn "Soy el sabio del laberinto. Adelante, escoge una opción"
    mostrarRecibirOpciones caminoSinSalida
