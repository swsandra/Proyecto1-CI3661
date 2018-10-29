module Laberinto
( Laberinto(..),
  caminoSinSalida,
  agregarLaberinto) where
{- Tipo de Datos: Laberinto 
                    Trifurcacion: izquierda, recto, derecho -}
data Laberinto = Trifurcacion (Maybe (Laberinto)) (Maybe (Laberinto)) (Maybe (Laberinto))
                | Tesoro String (Maybe (Laberinto))
                deriving (Show)

{-Instancias de Show y Read-}


{-Funciones de Construcción-}

{- Función que retorna un camino sin salida (Trifurcacion
    donde todos los caminos conducen a Nothing)-}
caminoSinSalida :: Laberinto
caminoSinSalida = Trifurcacion (Nothing) (Nothing) (Nothing)

{- Función que recibe un String con la descripción de un tesoro y
    un laberinto y retorna un Tesoro-}
crearTesoro :: [Char] -> Laberinto -> Laberinto
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
--OJO no hagas algo como let labx = agregarLaberinto labx lab1 'd' porque no hace nada

{-Funciones de Acceso-}

{- Función que recibe un laberinto y retorna el laberinto
    que comienza al voltear a la izquierda-}
voltearIzquierda :: Laberinto -> Laberinto
voltearIzquierda (Trifurcacion (Just(izq)) recto der) = izq
voltearIzquierda (Trifurcacion _ recto der) = error "No hay camino"

{- Función que recibe un laberinto y retorna el laberinto
    que comienza al voltear a la derecha-}
voltearDerecha :: Laberinto -> Laberinto
voltearDerecha (Trifurcacion izq recto (Just(der))) = der
voltearDerecha (Trifurcacion izq recto _) = error "No hay camino"

{- Función que recibe un laberinto y retorna el laberinto
    que comienza al seguir recto-}
irRecto :: Laberinto -> Laberinto
irRecto (Trifurcacion izq (Just(recto)) der) = recto
irRecto (Trifurcacion izq _ der) = error "No hay camino"

{- Función que recibe un laberinto y una ruta y retorna el
    laberinto que comienza en el punto al que conduce esa ruta
    La ruta es un string con d,i o r si es derecha, izquierda
    o recto respectivamente (e.g dirrdii)-}
recorrerLaberinto :: Laberinto -> [Char] -> Laberinto
recorrerLaberinto lab [] = lab
recorrerLaberinto lab (x:xs) | x == 'i' = recorrerLaberinto (voltearIzquierda lab) xs
                             | x == 'r' = recorrerLaberinto (irRecto lab) xs
                             | x == 'd' = recorrerLaberinto (voltearDerecha lab) xs
                             | otherwise = error "No se ha insertado la ruta correctamente"

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


