{-module Main (main) where
import Laberinto-}
import Data.Char (digitToInt, isDigit)

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

printMenu :: IO ()
printMenu = putStr [ (a ++ " .- " ++ b ++ "\n") | (a,b) <- opcionesDisponibles ]

-- Funcion para mostrar las opciones y recibir del user
mostrarRecibirOpciones :: IO ()
mostrarRecibirOpciones = do
    printMenu
    i:_ <- getLine
    if not ((isDigit i) && ((digitToInt i) `elem` [1..8]])) then do
        putStrLn "Operación incorrecta. Introduzca una opción válida."
    else
        putStr "do something"
    mostrarRecibirOpciones


main :: IO ()
main = do
    putStrLn "¡Bienvenido! Escoge una opción"
    mostrarRecibirOpciones