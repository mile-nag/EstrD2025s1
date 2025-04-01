{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Distribution.Simple.Setup (trueArg)

-- 1. Tipos recursivos simples

-- 1.1. Celdas con bolitas: Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:

data Color = Azul | Rojo
  deriving (Show)

data Celda = Bolita Color Celda | CeldaVacia
  deriving (Show)

{-
En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas,
podría ser la siguiente:
-}
celdaA = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia))) -- dos rojas, dos azules

celdaB = Bolita Rojo (Bolita Rojo (Bolita Rojo (Bolita Azul CeldaVacia))) -- tres rojas, una azul

celdaC = Bolita Azul (Bolita Rojo (Bolita Rojo (Bolita Rojo CeldaVacia)))

celdaD = Bolita Rojo (Bolita Rojo CeldaVacia)

-- Implementar las siguientes funciones sobre celdas:
-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia = 0
nroBolitas c (Bolita color celda) = unoSi (esElMismoColor c color) + nroBolitas c celda

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

esElMismoColor :: Color -> Color -> Bool
esElMismoColor Rojo Rojo = True
esElMismoColor Azul Azul = True
esElMismoColor _ _ = False

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner c CeldaVacia = Bolita c CeldaVacia
poner c (Bolita color celda) = Bolita color (poner c celda)

-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita color celda) =
  if esElMismoColor c color
    then celda
    else Bolita color (sacar c celda)

-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c celda = celda
ponerN n c celda = poner c (ponerN (n - 1) c celda)

-- 1.2. Camino hacia el tesoro:
-- Tenemos los siguientes tipos de datos
data Objeto = Cacharro | Tesoro
  deriving (Show)

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
  deriving (Show)

caminoA =
  Cofre
    [Tesoro, Cacharro, Tesoro, Cacharro]
    ( Nada
        ( Nada
            (Cofre [Cacharro] Fin)
        )
    ) -- 0 pasos hasta el tesoro -- 1 tesoro

caminoB =
  Cofre
    [Cacharro, Cacharro]
    ( Nada
        ( Nada
            (Cofre [Cacharro] Fin)
        )
    ) -- no hay tesoro -- 0 tesoros

caminoC =
  Cofre
    [Tesoro, Cacharro]
    ( Nada
        ( Nada
            ( Cofre
                [Cacharro]
                ( Cofre
                    [Tesoro, Cacharro, Tesoro, Cacharro]
                    ( Nada
                        (Nada (Cofre [Tesoro] Fin))
                    )
                )
            )
        )
    ) -- 0 pasos hasta el primer tesoro, 4 hasta el segundo o 7 hasta el tercero -- tres tesoros

caminoD =
  Cofre
    [Cacharro]
    ( Nada
        ( Nada
            ( Cofre
                [Cacharro]
                ( Cofre
                    [Cacharro, Cacharro]
                    ( Nada
                        (Nada (Cofre [Tesoro] Fin))
                    )
                )
            )
        )
    ) -- 8 pasos hasta el tesoro -- 1 tesoro

-- Definir las siguientes funciones:

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada camino) = hayTesoro camino
hayTesoro (Cofre obj camino) = hayUnTesoroEn obj || hayTesoro camino

hayUnTesoroEn :: [Objeto] -> Bool
hayUnTesoroEn [] = False
hayUnTesoroEn (o : os) = esTesoro o || hayUnTesoroEn os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro. Si un cofre con un tesoro está al principio del camino,
-- la cantidad de pasos a recorrer es 0. Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
-- Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Nada camino) = 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre objs camino) =
  if hayUnTesoroEn objs
    then 0
    else 1 + pasosHastaTesoro camino

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 camino = hayTesoroEnEsteLugar camino
hayTesoroEn n Fin = False
hayTesoroEn n (Nada camino) = hayTesoroEn (n - 1) camino
hayTesoroEn n (Cofre objs camino) = hayTesoroEn (n - 1) camino

hayTesoroEnEsteLugar :: Camino -> Bool
hayTesoroEnEsteLugar Fin = False
hayTesoroEnEsteLugar (Nada _) = False
hayTesoroEnEsteLugar (Cofre objs _) = hayUnTesoroEn objs

-- Indica si hay al menos n tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = cantDeTesorosTotales camino >= n

cantDeTesorosTotales :: Camino -> Int
cantDeTesorosTotales Fin = 0
cantDeTesorosTotales (Nada camino) = cantDeTesorosTotales camino
cantDeTesorosTotales (Cofre objs camino) = unoSi (hayUnTesoroEn objs) + cantDeTesorosTotales camino

{-
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
-}
{-
(desafío) cantTesorosEntre :: Int -> Int -> Camino -> Int
Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.
-}
