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
    Función que retorna un camino sin salida.
-}
caminoSinSalida :: Laberinto -- ^ Valor de retorno: Trifurcacion donde todos los caminos conducen a Nothing.
caminoSinSalida = Trifurcacion (Nothing) (Nothing) (Nothing)

{- |
    Función que crea un Tesoro.
-}
crearTesoro :: [Char] -- ^ String que posee la descripción de un Tesoro.
            -> Laberinto -- ^ Laberinto a agregar en el camino del tesoro.
            -> Laberinto -- ^ Valor de retorno: Tesoro.
crearTesoro s a = Tesoro s (Just a)

{- |
    Función que agrega un Laberinto en el camino de una Trifurcación, dicho camino es determinado 
    mediante un indicador. El indicador consiste en un char i, r o d que indica si se agrega el
    Laberinto a la izquierda, recto o a la derecha respectivamente.
-}
agregarLaberinto :: Laberinto -- ^ Trifurcación base donde se agregará un nuevo Laberinto.
                 -> Laberinto -- ^ Laberinto que se agregará a una Trifurcación.
                 -> Char -- ^ Indicador de cuál camino los relaciona.
                 -> Laberinto -- ^ Valor de retorno: Trifurcación en la cual el Laberinto fue agregado en la posición del camino dado por el indicador.
agregarLaberinto (Trifurcacion izq recto der) lab 'd' = Trifurcacion izq recto (Just lab)
agregarLaberinto (Trifurcacion izq recto der) lab 'i' = Trifurcacion (Just lab) recto der
agregarLaberinto (Trifurcacion izq recto der) lab 'r' = Trifurcacion izq (Just lab) der
agregarLaberinto (Trifurcacion izq recto der) lab _ = lab
agregarLaberinto (Tesoro str recto) lab 'r' = Tesoro str (Just lab)
agregarLaberinto (Tesoro str recto) lab _ = lab


{-Funciones de Acceso-}

{- |
    Función que voltea al camino de la izquierda de un Laberinto dado.
-}
voltearIzquierda :: Laberinto -- ^ Laberinto a recorrer.
                 -> Laberinto -- ^ Valor de retorno: Laberinto que comienza al voltear al camino de la izquierda.
voltearIzquierda (Trifurcacion (Just(izq)) recto der) = izq
voltearIzquierda (Trifurcacion _ recto der) = Trifurcacion (Nothing) recto der
voltearIzquierda (Tesoro str lab) = Tesoro str lab

{- |
    Función que voltea al camino de la derecha de un Laberinto dado.
-}
voltearDerecha :: Laberinto -- ^ Laberinto a recorrer.
               -> Laberinto -- ^ Valor de retorno: Laberinto que comienza al voltear al camino de la derecha.
voltearDerecha (Trifurcacion izq recto (Just(der))) = der
voltearDerecha (Trifurcacion izq recto _) = Trifurcacion izq recto (Nothing)
voltearDerecha (Tesoro str lab) = Tesoro str lab

{- |
    Función que continua por el camino recto de un Laberinto dado.
-}
irRecto :: Laberinto -- ^ Laberinto a recorrer.
        -> Laberinto -- ^ Valor de retorno: Laberinto que comienza al seguir por el camino recto.
irRecto (Trifurcacion izq (Just(recto)) der) = recto
irRecto (Trifurcacion izq _ der) = Trifurcacion izq (Nothing) der
irRecto (Tesoro str (Just(recto))) = recto
irRecto (Tesoro str _) = Tesoro str Nothing

{- | 
    Función que recorre una ruta en un Laberinto dado. La ruta se especifica con 
    indicadores que vienen dados por char i, r o d si es izquierda, recto o 
    derecha respectivamente (por ejemplo 'dird' representa el camino derecha-izquierda-recto-derecha).
-}
recorrerLaberinto :: Laberinto -- ^ Laberinto a recorrer.
                  -> [Char] -- ^ String que contiene una ruta especificada con indicadores.
                  -> Laberinto -- ^ Valor de retorno: Laberinto que comienza en el punto al que conduce esa ruta.
recorrerLaberinto lab [] = lab
recorrerLaberinto lab (x:xs) | x == 'i' = recorrerLaberinto (voltearIzquierda lab) xs
                             | x == 'r' = recorrerLaberinto (irRecto lab) xs
                             | x == 'd' = recorrerLaberinto (voltearDerecha lab) xs
                             | otherwise = recorrerLaberinto lab xs

{- Funciones auxiliares-}

{- |
    Función que construye un Laberinto a partir de una ruta y otro Laberinto, el cual es agregado en la 
    primera dirección que se especifica en la ruta.
-}
construirLaberinto :: Laberinto -- ^ Laberinto a agregar en la primera dirección de la ruta.
                   -> [Char] -- ^ String que contiene una ruta especificada con indicadores.
                   -> Laberinto -- ^ Valor de retorno: Trifurcación con el Laberinto parámetro agregado en la primera dirección de la ruta.
construirLaberinto lab str = if ((length str) == 1) then
                                agregarLaberinto (caminoSinSalida) lab (head str)
                             else
                                agregarLaberinto (caminoSinSalida) (construirLaberinto lab (tail str)) (head str)

{- |
    Función que evalúa si una Trifurcación es normal, un camino sin salida o una habitación con un Tesoro.
-}
evaluarLaberinto :: Laberinto -- ^ Laberinto a evaluar.
                 -> Char -- ^ Valor de retorno: Caracter que indica qué tipo de Trifurcación es.
evaluarLaberinto (Trifurcacion Nothing Nothing Nothing) = 'S'
evaluarLaberinto (Trifurcacion izq recto der) = 'T'
evaluarLaberinto (Tesoro str recto) = 'E'

{- |
    Función que retorna la descripción de un Tesoro.
-}
getTesoroStr :: Laberinto -- ^ Tesoro del cual se desea saber la descripción.
             -> String -- ^ Valor de retorno: String que contiene la descripción del Tesoro.
getTesoroStr (Tesoro str recto) = str

-- 
{- |
    Funcion que recibe un laberinto y un string, y recorre el laberinto hasta acabar el string
    o hasta llegar a una pared, donde construye el laberinto con el resto del string y lo agrega
    en la direccion correspondiente a la cabeza del string restante. Más detalles en README.txt.
-}
reportarParedAbierta :: Laberinto -- ^ Laberinto a recorrer.
                     -> [Char] -- ^ Ruta a seguir.
                     -> Laberinto -- ^ Valor de retorno: 
reportarParedAbierta lab [] = lab
reportarParedAbierta lab str =
                        if (recorrerLaberinto lab ([head str]) == lab && ((head str) == 'd' || (head str) == 'r' || (head str) == 'i')) then
                            if ((length str) == 1) then
                                agregarLaberinto (lab) caminoSinSalida (head str)
                            else
                                agregarLaberinto (lab) (construirLaberinto caminoSinSalida (tail str)) (head str)
                        else
                            reportarParedAbierta (recorrerLaberinto lab ([head str])) (tail str)

{- |
    Funcion que recibe un laberinto y dos string, y recorre el laberinto hasta acabar el string
    donde toma la cabeza del segundo string (un char) para reemplazar el laberinto en esa direccion
    por Nothing. Más detalles en README.txt.
-}
reportarDerrumbe :: Laberinto -- ^ Laberinto a recorrer.
                 -> [Char] -- ^ Ruta a seguir.
                 -> [Char] -- ^ String cuyo primer caracter es la dirección a derrumbar.
                 -> Laberinto -- ^ Valor de retorno:
reportarDerrumbe (Trifurcacion izq rect der) [] [] = Trifurcacion izq rect der
reportarDerrumbe (Trifurcacion izq rect der) str [] = Trifurcacion izq rect der
reportarDerrumbe (Trifurcacion izq rect der) [] char = 
        case (head char) of
            'i' -> Trifurcacion (Nothing) rect der
            'r' -> Trifurcacion izq (Nothing) der
            'd' -> Trifurcacion izq rect (Nothing)
reportarDerrumbe (Trifurcacion izq rect der) str char=
        reportarDerrumbe (recorrerLaberinto (Trifurcacion izq rect der) str) [] char

--recorrerRecordando :: Laberinto -> [Char] -> [Char] -> Laberinto
-- recorrerRecordando (Trifurcacion izq rect der) str strOpt =
--     if (length str == 0) then
--         agregarLaberinto (Tesoro strOpt Nothing) (Trifurcacion izq rect der) 'r'
--     else do
--         let labARecorrer = recorrerLaberinto (Trifurcacion izq rect der) (head [str])
--         return (agregarLaberinto (Trifurcacion izq rect der) (recorrerRecordando labARecorrer (tail str) strOpt) (head str))
-- recorrerRecordando (Tesoro strTesoro recto) str strOpt =
--     if (length str == 0) then
--         recto
--     else do
--         let labARecorrer = recorrerLaberinto (Tesoro strTesoro recto) (head str)
--         return (agregarLaberinto (Tesoro strTesoro recto) (recorrerRecordando labARecorrer (tail str) strOpt) (head str))
-- recorrerRecordando _ str strOpt = Nothing

{- |
    Función auxiliar que escribe un Laberinto en un archivo.
-}
escribirLaberinto :: FilePath -- ^ Ruta del archivo a escribir.
                  -> Laberinto -- ^ Laberinto que se desea escribir.
                  -> IO() -- ^ Valor de retorno: IO.
escribirLaberinto path laberinto = writeFile path $ show laberinto