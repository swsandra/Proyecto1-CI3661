module Laberinto
( Laberinto(..),
  caminoSinSalida,
  crearTesoro,
  agregarLaberinto,
  voltearIzquierda,
  voltearDerecha,
  irRecto,
  recorrerLaberinto,
  construirLaberinto,
  evaluarLaberinto,
  getTesoroStr,
  reportarParedAbierta,
  reportarDerrumbe,
  leerLaberinto,
  escribirLaberinto
)
where

import System.IO

-- | Tipo de datos Laberinto.
data Laberinto 
    -- | Constructor de una trifurcación de un laberinto en maybe laberintos
    -- con caminos en el orden izquierda, recto y derecha.
    = Trifurcacion (Maybe (Laberinto)) (Maybe (Laberinto)) (Maybe (Laberinto))
    -- | Constructor de un tesoro en el camino de un laberinto.
    | Tesoro String (Maybe (Laberinto))
    deriving (Show, Read, Eq)

{-Funciones de Construcción de laberintos-}

{- |
    Función que retorna un camino sin salida, es decir una Trifurcacion
    donde todos los caminos conducen a Nothing.
-}
caminoSinSalida :: Laberinto
caminoSinSalida = Trifurcacion (Nothing) (Nothing) (Nothing)

{- |
    Función que recibe un String con la descripción de un Tesoro y
    un Laberinto y retorna un Tesoro.
-}
crearTesoro :: [Char] -> Laberinto -> Laberinto
crearTesoro s a = Tesoro s (Just a)

{- |
    Función que recibe una Trifurcacion, un Laberinto y un indicador de 
    cual camino los relaciona y retorna una Trifurcacion en la que se indica 
    que dicho camino especificado en el indicador conduce al Laberinto dado 
    como parámetro.
    El indicador viene dado por un char i, r o d que indica si se agrega el
    Laberinto a la izquierda, recto o a la derecha respectivamente.
-}
agregarLaberinto :: Laberinto -> Laberinto -> Char -> Laberinto
agregarLaberinto (Trifurcacion izq recto der) lab 'd' = Trifurcacion izq recto (Just lab)
agregarLaberinto (Trifurcacion izq recto der) lab 'i' = Trifurcacion (Just lab) recto der
agregarLaberinto (Trifurcacion izq recto der) lab 'r' = Trifurcacion izq (Just lab) der
--agregarLaberinto (Trifurcacion izq recto der) lab _ = error "Se debe especificar dirección del camino ('d', 'i' o 'r')"
agregarLaberinto (Trifurcacion izq recto der) lab _ = lab
agregarLaberinto (Tesoro str recto) lab 'r' = Tesoro str (Just lab)
agregarLaberinto (Tesoro str recto) lab _ = lab


{-Funciones de Acceso-}

{- |
    Función que recibe un Laberinto y retorna el Laberinto
    que comienza al voltear al camino de la izquierda.
-}
voltearIzquierda :: Laberinto -> Laberinto
voltearIzquierda (Trifurcacion (Just(izq)) recto der) = izq
voltearIzquierda (Trifurcacion _ recto der) = Trifurcacion (Nothing) recto der
voltearIzquierda (Tesoro str lab) = Tesoro str lab

{- |
    Función que recibe un Laberinto y retorna el Laberinto
    que comienza al voltear al camino de la derecha.
-}
voltearDerecha :: Laberinto -> Laberinto
voltearDerecha (Trifurcacion izq recto (Just(der))) = der
voltearDerecha (Trifurcacion izq recto _) = Trifurcacion izq recto (Nothing)
voltearDerecha (Tesoro str lab) = Tesoro str lab

{- |
    Función que recibe un Laberinto y retorna el Laberinto
    que comienza al seguir por el camino recto.
-}
irRecto :: Laberinto -> Laberinto
irRecto (Trifurcacion izq (Just(recto)) der) = recto
irRecto (Trifurcacion izq _ der) = Trifurcacion izq (Nothing) der
irRecto (Tesoro str (Just(recto))) = recto
irRecto (Tesoro str _) = Tesoro str Nothing

{- | 
    Función que recibe un Laberinto y String que contiene una ruta
    y retorna el Laberinto que comienza en el punto al que conduce 
    esa ruta. La ruta se especifica con indicadores que vienen dados
    por char i, r o d si es izquierda, recto o derecha respectivamente
    (por ejemplo 'dird' representa el camino derecha-izquierda-recto-derecha).
-}
recorrerLaberinto :: Laberinto -> [Char] -> Laberinto
recorrerLaberinto lab [] = lab
recorrerLaberinto lab (x:xs) | x == 'i' = recorrerLaberinto (voltearIzquierda lab) xs
                             | x == 'r' = recorrerLaberinto (irRecto lab) xs
                             | x == 'd' = recorrerLaberinto (voltearDerecha lab) xs
                            -- | otherwise = error "No se ha insertado la ruta correctamente"
                             | otherwise = recorrerLaberinto lab xs




{- Funciones auxiliares-}

construirLaberinto :: Laberinto -> [Char] -> Laberinto
construirLaberinto lab str = if ((length str) == 1) then
                                agregarLaberinto (caminoSinSalida) lab (head str)
                             else
                                agregarLaberinto (caminoSinSalida) (construirLaberinto lab (tail str)) (head str)

evaluarLaberinto :: Laberinto -> Char
evaluarLaberinto (Trifurcacion Nothing Nothing Nothing) = 'S'
evaluarLaberinto (Trifurcacion izq recto der) = 'T'
evaluarLaberinto (Tesoro str recto) = 'E'

getTesoroStr :: Laberinto -> String
getTesoroStr (Tesoro str recto) = str

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

--Funcion para leer un laberinto de un archivo de texto
--leerLaberinto :: FilePath -> Laberinto --Esto retorna IO Laberinto, puedes pegarlo directamente en la opcion
--para  evitar el problema
leerLaberinto path = do
    archivo <- openFile path ReadMode
    contents <- hGetContents archivo
    return (read contents :: Laberinto) --Comenta esto y descomenta el de abajo cuando vayas a usarlo
{-    let x = read contents in 
        do
            FUNCION QUE CARGA EL LABERINTO, ESTA EN X (para Elvin era cliente [] x, x es el laberinto)
            hClose archivo-}

escribirLaberinto :: FilePath -> Laberinto -> IO()
escribirLaberinto path laberinto = writeFile path $ show laberinto

--Pruebas con los laberintos de la prueba anterior
--let lab4 = voltearIzquierda lab1
--En lo anterior solo cambio los laberintos y veo si está bien
--Para recorrer laberinto, usaré esta instancia fea
--let lab5 = agregarLaberinto lab2 lab1 'i'
--Para este ejemplo puedes ver mas fácil cómo lo recorre aquí
{- Trifurcacion 
    (Just (Trifurcacion 
            (Just (Trifurcacion 
                    Nothing 
                    Nothing 
                    (Just (Trifurcacion 
                            Nothing 
                            Nothing 
                            Nothing)))) 
            (Just (Trifurcacion 
                    Nothing 
                    Nothing 
                    Nothing)) 
            (Just (Trifurcacion 
                    Nothing 
                    Nothing 
                    (Just (Trifurcacion 
                            Nothing 
                            Nothing 
                            Nothing))))))
     (Just (Tesoro "Tesoro1" 
            (Just (Trifurcacion 
                    Nothing 
                    Nothing 
                    Nothing)))) 
     (Just (Tesoro "Tesoro2" 
            (Just (Trifurcacion 
                    Nothing 
                    Nothing 
                    Nothing))))

Otro ejemplo con mas anidacion (Just(caminoSinSalida))
let lab6 = Trifurcacion Nothing Nothing (Just(Trifurcacion (Just(Trifurcacion (Just(Trifurcacion (Just(caminoSinSalida)) Nothing (Just(Trifurcacion (Just(caminoSinSalida)) Nothing (Just(caminoSinSalida)))))) Nothing Nothing)) Nothing (Just(Trifurcacion (Just(caminoSinSalida)) Nothing (Just(caminoSinSalida))))))
 Trifurcacion 
    Nothing 
    Nothing 
    (Just (Trifurcacion 
        (Just (Trifurcacion 
            (Just (Trifurcacion 
                (Just (Trifurcacion 
                        Nothing 
                        Nothing 
                        Nothing)) 
                Nothing 
                (Just (Trifurcacion 
                    (Just (Trifurcacion 
                            Nothing 
                            Nothing 
                            Nothing)) 
                    Nothing
                    (Just (Trifurcacion 
                            Nothing 
                            Nothing 
                            Nothing)))))) 
            Nothing 
            Nothing)) 
        Nothing 
        (Just (Trifurcacion 
                (Just (Trifurcacion 
                        Nothing 
                        Nothing 
                        Nothing)) 
                Nothing 
                (Just (Trifurcacion 
                        Nothing 
                        Nothing 
                        Nothing))))))
-}



