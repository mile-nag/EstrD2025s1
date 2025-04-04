-- 1. Pizzas
-- Tenemos los siguientes tipos de datos:
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

data Pizza
  = Prepizza
  | Capa Ingrediente Pizza
  deriving (Show)

data Ingrediente
  = Salsa
  | Queso
  | Jamon
  | Aceitunas Int
  deriving (Show)

-- Definir las siguientes funciones:

-- Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

-- Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i : is) = Capa i (armarPizza is)

-- Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) =
  if esJamon i
    then p
    else Capa i (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

pizza1 = Capa Salsa (Capa Queso (Capa Jamon (Capa (Aceitunas 8) Prepizza)))

-- Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa i p) = esQuesoOSalsa i && tieneSoloSalsaYQueso p

esQuesoOSalsa :: Ingrediente -> Bool
esQuesoOSalsa Queso = True
esQuesoOSalsa Salsa = True
esQuesoOSalsa _ = False

pizza2 = Capa Salsa (Capa Queso Prepizza)

-- Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarSiSonAceitunas i) (duplicarAceitunas p)

duplicarSiSonAceitunas :: Ingrediente -> Ingrediente
duplicarSiSonAceitunas (Aceitunas int) = Aceitunas (int + int)
duplicarSiSonAceitunas ingrediente = ingrediente

pizza3 = Capa Salsa (Capa Queso (Capa Jamon (Capa (Aceitunas 1) Prepizza)))

-- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p : ps) = (contarCapas p, p) : cantCapasPorPizza ps

contarCapas :: Pizza -> Int
contarCapas Prepizza = 0
contarCapas (Capa _ p) = 1 + contarCapas p

-- ----------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 2. Mapa de tesoros (con bifurcaciones)
-- Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y cada cofre tiene un objeto, que puede ser chatarra o un tesoro.
data Dir = Izq | Der
  deriving (Show)

data Objeto = Tesoro | Chatarra
  deriving (Show)

data Cofre = Cofre [Objeto]
  deriving (Show)

data Mapa
  = Fin Cofre
  | Bifurcacion Cofre Mapa Mapa
  deriving (Show)