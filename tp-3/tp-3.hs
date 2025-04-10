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
unoSi _ = 0

esElMismoColor :: Color -> Color -> Bool
esElMismoColor Rojo Rojo = True
esElMismoColor Azul Azul = True
esElMismoColor _ _ = False

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner c celda = Bolita c celda

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
hayTesoroEnEsteLugar (Cofre objs _) = hayUnTesoroEn objs
hayTesoroEnEsteLugar _ = False

-- Indica si hay al menos n tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = cantDeTesorosTotales camino >= n

cantDeTesorosTotales :: Camino -> Int
cantDeTesorosTotales Fin = 0
cantDeTesorosTotales (Nada camino) = cantDeTesorosTotales camino
cantDeTesorosTotales (Cofre objs camino) = unoSi (hayUnTesoroEn objs) + cantDeTesorosTotales camino

{-
(desafío) cantTesorosEntre :: Int -> Int -> Camino -> Int
Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.
-}

-- 2. Tipos arbóreos
-- 2.1. Árboles binarios
-- Dada esta definición para árboles binarios defina las siguientes funciones utilizando recursión estructural según corresponda:

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
  deriving (Show)

-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

n1 :: Tree Int
n1 = NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT) (NodeT 4 (NodeT 5 EmptyT EmptyT) EmptyT)

{-
                           NodeT 1
                        /           \
                  NodeT 2          NodeT 4
                 /       \        /       \
            NodeT 3    EmptyT   NodeT 5   EmptyT
           /       \           /       \
      EmptyT     EmptyT     EmptyT   EmptyT
-}

n2 :: Tree Int
n2 = NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT) (NodeT 4 (NodeT 5 EmptyT EmptyT) (NodeT 2 (NodeT 3 EmptyT EmptyT) (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT)))

{-
    |                             NodeT 1                                       |
    |                        /               \                                  |
    |                  NodeT 2                 NodeT 4                          |
    |                 /       \              /         \                        |
    |          NodeT 3    EmptyT     NodeT 5              NodeT 2               |
    |         /       \              /     \             /       \              |
    |    EmptyT     EmptyT       EmptyT  EmptyT      NodeT 3   NodeT 2          |
    |                                                /     \    /     \         |
    |                                            EmptyT EmptyT NodeT 3 EmptyT   |
    |                                                         /     \           |
    |                                                    EmptyT     EmptyT      |

-}
-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n + n) (mapDobleT t1) (mapDobleT t2)

-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT :: (Eq a) => a -> Tree a -> Bool
perteneceT y EmptyT = False
perteneceT y (NodeT x t1 t2) = x == y || perteneceT y t1 || perteneceT y t2

-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
aparicionesT :: (Eq a) => a -> Tree a -> Int
aparicionesT y EmptyT = 0
aparicionesT y (NodeT x t1 t2) = unoSi (y == x) + aparicionesT y t1 + aparicionesT y t2

-- Dado un árbol devuelve los elementos que se encuentran en sus hojas. NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x t1 t2) =
  if esEmpty t1 && esEmpty t2
    then [x]
    else leaves t1 ++ leaves t2

esEmpty :: Tree a -> Bool
esEmpty EmptyT = True
esEmpty _ = False

-- Dado un árbol devuelve su altura. Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad de niveles del árbol1.
-- La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x ti td) = NodeT x (mirrorT td) (mirrorT ti)

-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order. Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz y luego los elementos del hijo derecho.
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x t1 t2) = toList t1 ++ [x] ++ toList t2

n3 :: Tree Int
n3 =
  NodeT
    4
    ( NodeT
        2
        (NodeT 1 EmptyT EmptyT)
        (NodeT 3 EmptyT EmptyT)
    )
    ( NodeT
        6
        (NodeT 5 EmptyT EmptyT)
        (NodeT 7 EmptyT EmptyT)
    )

{-
                       4          [[4], [2,6], [1,3,5,7]]
                      / \
                     2   6      si hago toList test_5 --> RESULTADO [1,2,3,4,5,6,7]
                    /\   /\
                   1  3 5  7
-}

-- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1. Nota: El primer nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT _ t1 t2) = levelN (n - 1) t1 ++ levelN (n - 1) t2

-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) = [x] : combinarNiveles (listPerLevel t1) (listPerLevel t2)

combinarNiveles :: [[a]] -> [[a]] -> [[a]]
combinarNiveles [] yss = yss
combinarNiveles xss [] = xss
combinarNiveles (xs : xss) (ys : yss) = (xs ++ ys) : combinarNiveles xss yss

-- Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) =
  if heightT t1 < heightT t2
    then x : ramaMasLarga t2
    else x : ramaMasLarga t1

-- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) =
  [[x]] ++ (agregarAlPrincipio x (todosLosCaminos t1) ++ agregarAlPrincipio x (todosLosCaminos t2))

agregarAlPrincipio :: a -> [[a]] -> [[a]]
agregarAlPrincipio x [] = []
agregarAlPrincipio x (y : ys) = (x : y) : agregarAlPrincipio x ys

{-
ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o sea, de la raíz hasta la hoja), o sea, por ejemplo
todosLosCaminos (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT) (NodeT 4 (NodeT 5 EmptyT EmptyT) EmptyT))
= [ [1], [1,2], [1,2,3], [1,4], [1,4,5] ]
OBSERVACIÓN: puede resultar interesante plantear otra función, variación de ésta para devolver solamente los caminos maximales.
-}

--  (2.2) EXPPRESIONES ARITMETICAS

data ExpA
  = Valor Int
  | Sum ExpA ExpA
  | Prod ExpA ExpA
  | Neg ExpA
  deriving (Show)

--  1. Dada una expresión aritmética devuelve el resultado evaluarla
eval :: ExpA -> Int
eval (Valor n) = n
eval (Neg exp) = -(eval exp)
eval (Sum exp1 exp2) = eval exp1 + eval exp2
eval (Prod exp1 exp2) = eval exp1 * eval exp2

{-
  2. Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
  notación matemática convencional):
  a) 0 + x = x + 0 = x
  b) 0 * x = x * 0 = 0
  c) 1 * x = x * 1 = x
  d) - (- x) = x
-}
nro = Sum (Sum (Valor 2) (Valor 3)) (Prod (Valor 0) (Sum (Valor 2) (Valor 3)))

simplificar :: ExpA -> ExpA
simplificar (Prod e1 e2) = simplificarProd (simplificar e1) (simplificar e2)
simplificar (Sum e1 e2) = simplificarSum (simplificar e1) (simplificar e2)
simplificar (Neg exp) = simplificarNeg (simplificar exp)
simplificar exp = exp

simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum exp (Valor 0) = exp
simplificarSum (Valor 0) exp = exp
simplificarSum exp expp = Sum exp expp

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd _ (Valor 0) = Valor 0
simplificarProd (Valor 0) _ = Valor 0
simplificarProd exp (Valor 1) = exp
simplificarProd (Valor 1) e = e
simplificarProd exp expp = Prod exp expp

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg exp) = exp
simplificarNeg exp = Neg exp
