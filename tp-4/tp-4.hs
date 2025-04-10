-- 1. Pizzas
-- Tenemos los siguientes tipos de datos:
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Eta reduce" #-}
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
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _ = False

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
cantCapasPorPizza (p : ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps

-- ----------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 2. Mapa de tesoros (con bifurcaciones)
-- Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y cada cofre tiene un objeto, que puede ser chatarra o un tesoro.

data Dir = Izq | Der
  deriving (Show)

data Objeto = Tesoro | Chatarra
  deriving (Show)

data Cofre = Cofre [Objeto]
  deriving (Show)

data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
  deriving (Show)

-- Definir las siguientes operaciones:
-- Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre) = hayTesoroAca (objetosDelCofre cofre)
hayTesoro (Bifurcacion cofre m1 m2) = hayTesoroAca (objetosDelCofre cofre) || hayTesoro m1 || hayTesoro m2

-- Aux: indica si en la lista actual de objetos del cofre hay tesoros.
hayTesoroAca :: [Objeto] -> Bool
hayTesoroAca [] = False
hayTesoroAca (o : os) = esTesoro o || hayTesoroAca os

-- Aux: Funcion observadora, devuelve los objetos de un cofre.
objetosDelCofre :: Cofre -> [Objeto]
objetosDelCofre (Cofre objs) = objs

-- Aux: Denota si el objeto dado es un tesoro.
esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

-- EJEMPLOS
-- mapa con solo Chatarra (sin tesoro)
mapa1 :: Mapa
mapa1 = Fin (Cofre [Chatarra, Chatarra])

-- mapa con un Tesoro al final
mapa2 :: Mapa
mapa2 = Fin (Cofre [Chatarra, Tesoro])

-- mapa con bifurcación, tesoro en la bifurcación izquierda
mapa3 :: Mapa
mapa3 = Bifurcacion (Cofre [Chatarra]) (Fin (Cofre [Tesoro])) (Fin (Cofre [Chatarra]))

--  mapa con tesoro en el cofre de la bifurcación principal
mapa4 :: Mapa
mapa4 = Bifurcacion (Cofre [Tesoro]) (Fin (Cofre [Chatarra])) (Fin (Cofre [Chatarra]))

-- Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn ds (Fin cofre) = null ds && hayTesoroAca (objetosDelCofre cofre)
hayTesoroEn [] (Bifurcacion cofre m1 m2) = hayTesoroAca (objetosDelCofre cofre)
hayTesoroEn (d : ds) (Bifurcacion _ m1 m2) = hayTesoroEn ds (seguirMapa d m1 m2)

seguirMapa :: Dir -> Mapa -> Mapa -> Mapa
seguirMapa Izq m1 _ = m1
seguirMapa Der _ m2 = m2

-- Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin _) = []
caminoAlTesoro (Bifurcacion _ m1 m2) =
  if hayTesoro m1
    then Izq : caminoAlTesoro m1
    else Der : caminoAlTesoro m2

mapa5 :: Mapa
mapa5 =
  Bifurcacion
    (Cofre [Tesoro])
    (Fin (Cofre []))
    ( Bifurcacion
        (Cofre [Tesoro])
        (Fin (Cofre [Chatarra]))
        ( Bifurcacion
            (Cofre [Tesoro])
            (Fin (Cofre []))
            ( Bifurcacion
                (Cofre [Tesoro])
                (Fin (Cofre [Chatarra]))
                (Fin (Cofre []))
            )
        )
    )

{-
                                            Bifurcacion C tesoro
                                            /                 \
                                    Fin c []                 Bifurcacion c tesoro
                                                              /                \
                                                        Fin c chat         Bifurcacion c tesoro
                                                                            /                 \
                                                                         Fin c []         Bifurcacion c tesoro
                                                                                          /                  \
                                                                                        Fin                  Fin
-}
-- Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) =
  if esElCaminoMasLargo m1 m2
    then Izq : caminoDeLaRamaMasLarga m1
    else Der : caminoDeLaRamaMasLarga m2

esElCaminoMasLargo :: Mapa -> Mapa -> Bool
esElCaminoMasLargo m1 m2 = largo m1 >= largo m2

largo :: Mapa -> Int
largo (Fin _) = 0
largo (Bifurcacion _ m1 m2) = 1 + max (largo m1) (largo m2)

mapa6 :: Mapa
mapa6 =
  Bifurcacion
    (Cofre [])
    (Fin (Cofre []))
    ( Bifurcacion
        (Cofre [])
        (Fin (Cofre []))
        ( Bifurcacion
            (Cofre [])
            ( Bifurcacion
                (Cofre [])
                ( Bifurcacion
                    (Cofre [])
                    (Fin (Cofre []))
                    ( Bifurcacion
                        (Cofre [])
                        ( Bifurcacion
                            (Cofre [])
                            ( Bifurcacion
                                (Cofre [])
                                (Fin (Cofre [Tesoro]))
                                (Fin (Cofre []))
                            )
                            (Fin (Cofre []))
                        )
                        (Fin (Cofre []))
                    )
                )
                (Fin (Cofre []))
            )
            (Fin (Cofre []))
        )
    )

{- Mapa6

                b
              /   \
            f       b
                  /   \
                f       b
                      /   \
                    b       f
                   / \
                 b     b
                / \   / \
              f    b  f   f
                  / \
              f[T]   f

caminoDeLaRamaMasLarga [Der, Der, Izq, Izq, Der, Izq, Izq]
-}

-- Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin cofre) = [objetosDelCofre cofre]
tesorosPorNivel (Bifurcacion cofre m1 m2) = objetosDelCofre cofre : combinarNiveles (tesorosPorNivel m1) (tesorosPorNivel m2)

combinarNiveles :: [[a]] -> [[a]] -> [[a]]
combinarNiveles [] yss = yss
combinarNiveles xss [] = xss
combinarNiveles (xs : xss) (ys : yss) = (xs ++ ys) : combinarNiveles xss yss

mapa7 :: Mapa
mapa7 =
  Bifurcacion
    (Cofre [Tesoro])
    (Fin (Cofre [Chatarra]))
    ( Bifurcacion
        (Cofre [Tesoro])
        (Fin (Cofre [Chatarra]))
        ( Bifurcacion
            (Cofre [Tesoro])
            (Fin (Cofre [Chatarra]))
            ( Bifurcacion
                (Cofre [Tesoro])
                (Fin (Cofre [Chatarra]))
                (Fin (Cofre [Tesoro]))
            )
        )
    )

{-
               BifurcaciOn
               /         \
           Fin           BifurcaciOn
         [Chatarra]        /       \
                        Fin     BifurcaciOn
                    [Chatarra]    /        \
                               Fin       BifurcaciOn
                           [Chatarra]      /        \
                                        Fin         Fin
                                    [Chatarra]    [Tesoro]
-}

-- Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = []
todosLosCaminos (Bifurcacion _ m1 m2) =
  [Izq] : agregarAlPrincipio Izq (todosLosCaminos m1) ++ [Der] : agregarAlPrincipio Der (todosLosCaminos m2)

agregarAlPrincipio :: Dir -> [[Dir]] -> [[Dir]]
agregarAlPrincipio _ [] = []
agregarAlPrincipio d (dir : dirs) = (d : dir) : agregarAlPrincipio d dirs

{-
3. Nave Espacial

Modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
es la siguiente:
-}

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
  deriving (Show)

data Barril = Comida | Oxigeno | Torpedo | Combustible
  deriving (Show)

data Sector = S SectorId [Componente] [Tripulante]
  deriving (Show)

type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
  deriving (Show)

data Nave = N (Tree Sector)
  deriving (Show)

-- Implementar las siguientes funciones utilizando recursión estructural:

-- Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores (N ts) = sectoresT ts

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT = []
sectoresT (NodeT s s1 s2) = idDelSector s : sectoresT s1 ++ sectoresT s2

idDelSector :: Sector -> SectorId
idDelSector (S id _ _) = id

nave1 :: Nave
nave1 =
  N
    ( NodeT
        (S "Puente" [Motor 5] ["Carmen"])
        ( NodeT
            (S "Cocina" [Almacen [Comida, Comida, Oxigeno]] ["Juan"])
            EmptyT
            EmptyT
        )
        ( NodeT
            (S "Armeria" [LanzaTorpedos, Almacen [Torpedo, Torpedo]] ["Sandra"])
            ( NodeT
                (S "Bodega" [Almacen [Comida, Combustible]] ["Raul"])
                EmptyT
                EmptyT
            )
            ( NodeT
                (S "Sala de Maquinas" [Motor 8, Almacen [Combustible, Oxigeno]] ["Noemi"])
                EmptyT
                EmptyT
            )
        )
    )

-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota: el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N sectores) = poderDePropulsionT sectores

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT EmptyT = 0
poderDePropulsionT (NodeT sector s1 s2) = poderDePropulsionDelSector sector + poderDePropulsionT s1 + poderDePropulsionT s2

poderDePropulsionDelSector :: Sector -> Int
poderDePropulsionDelSector (S _ componentes _) = poderDePropulsionDeLosMotoresEn componentes

poderDePropulsionDeLosMotoresEn :: [Componente] -> Int
poderDePropulsionDeLosMotoresEn [] = 0
poderDePropulsionDeLosMotoresEn (c : cs) =
  if esMotor c
    then poder c
    else poderDePropulsionDeLosMotoresEn cs

esMotor :: Componente -> Bool
esMotor (Motor _) = True
esMotor _ = False

poder :: Componente -> Int
poder (Motor poder) = poder

nave2 :: Nave
nave2 =
  N
    ( NodeT
        (S "A" [LanzaTorpedos, Motor 10] ["Rena", "Gonza"])
        ( NodeT
            (S "B" [LanzaTorpedos, Motor 20] ["Azu", "Mile"])
            EmptyT
            EmptyT
        )
        ( NodeT
            (S "C" [Motor 30] ["Pabli", "Ami"])
            EmptyT
            ( NodeT
                (S "D" [Motor 40, Almacen [Comida, Oxigeno, Combustible]] ["Mar"])
                EmptyT
                EmptyT
            )
        )
    )

nave3 :: Nave
nave3 =
  N
    ( NodeT
        (S "E" [LanzaTorpedos, Motor 1] ["Gonza"])
        ( NodeT
            (S "F" [LanzaTorpedos, Motor 2, Almacen [Oxigeno, Combustible]] ["Azu"])
            EmptyT
            EmptyT
        )
        ( NodeT
            (S "G" [Motor 3] ["Pabli", "Ami"])
            EmptyT
            ( NodeT
                (S "H" [Almacen [Comida, Torpedo, Combustible]] ["Mile"])
                EmptyT
                EmptyT
            )
        )
    )

-- poderDePropulsion nave2  ---> 100
-- poderDePropulsion nave3  ---> 6

-- Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT s t1 t2) = barrilesDelSector s ++ barrilesT t1 ++ barrilesT t2

barrilesDelSector :: Sector -> [Barril]
barrilesDelSector (S _ c _) = barrilesEnC c

barrilesEnC :: [Componente] -> [Barril]
barrilesEnC [] = []
barrilesEnC (c : cs) = barrilC c ++ barrilesEnC cs

barrilC :: Componente -> [Barril]
barrilC (Almacen b) = b
barrilC _ = []

-- Propósito: Añade una lista de componentes a un sector de la nave. Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N t) = N (agregarASectorT cs id t)

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT _ _ EmptyT = EmptyT
agregarASectorT cs id (NodeT sector t1 t2) =
  NodeT
    (agregarAComponente sector cs id)
    (agregarASectorT cs id t1)
    (agregarASectorT cs id t2)

agregarAComponente :: Sector -> [Componente] -> SectorId -> Sector
agregarAComponente (S id cs1 t) cs2 sid =
  if id == sid
    then S id (cs1 ++ cs2) t
    else S id cs1 t

nave4 :: Nave
nave4 =
  N
    ( NodeT
        (S "A" [LanzaTorpedos] ["Rena", "Gonza"])
        ( NodeT
            (S "B" [LanzaTorpedos] ["Azu", "Mile"])
            EmptyT
            EmptyT
        )
        ( NodeT
            (S "C" [] ["Pabli", "Ami"])
            EmptyT
            ( NodeT
                (S "D" [Motor 40, Almacen [Comida, Oxigeno, Combustible]] ["Mar"])
                EmptyT
                EmptyT
            )
        )
    )

-- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t ss (N sectores) = N (asignarTripulanteAT t ss sectores)

asignarTripulanteAT :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteAT t ss EmptyT = EmptyT
asignarTripulanteAT t ss (NodeT s si sd) =
  NodeT
    (agregarTripulanteASector t ss s)
    (asignarTripulanteAT t ss si)
    (asignarTripulanteAT t ss sd)

agregarTripulanteASector :: Tripulante -> [SectorId] -> Sector -> Sector
agregarTripulanteASector t ss (S id comps trips) =
  if id `elem` ss
    then S id comps (t : trips)
    else S id comps trips

nave5 :: Nave
nave5 =
  N
    ( NodeT
        (S "A" [LanzaTorpedos] [])
        ( NodeT
            (S "B" [LanzaTorpedos] [])
            EmptyT
            EmptyT
        )
        ( NodeT
            (S "C" [] [])
            EmptyT
            ( NodeT
                (S "D" [Motor 40, Almacen [Comida, Oxigeno, Combustible]] [])
                EmptyT
                EmptyT
            )
        )
    )

-- asignarTripulanteA "Random" ["A", "B", "C", "1", "2"] nave5
-- respuesta --> N (NodeT (S "A" [LanzaTorpedos] ["Random"]) (NodeT (S "B" [LanzaTorpedos] ["Random"]) EmptyT EmptyT) (NodeT (S "C" [] ["Random"]) EmptyT (NodeT (S "D" [Motor 40,Almacen [Comida,Oxigeno,Combustible]] []) EmptyT EmptyT)))

-- Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N ts) = sectoresAsignadosT t ts

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosT t EmptyT = []
sectoresAsignadosT t (NodeT s si sd) = idDelSectorSi_Pertenece t s ++ sectoresAsignadosT t si ++ sectoresAsignadosT t sd

idDelSectorSi_Pertenece :: Tripulante -> Sector -> [SectorId]
idDelSectorSi_Pertenece t (S sid _ tripulantes) =
  if t `elem` tripulantes
    then [sid]
    else []

nave6 :: Nave
nave6 =
  N
    ( NodeT
        (S "A" [LanzaTorpedos] ["Carmen"])
        ( NodeT
            (S "B" [LanzaTorpedos] ["Carmen", "Marta"])
            EmptyT
            EmptyT
        )
        ( NodeT
            (S "C" [] ["Sol", "Sofia", "Carmen"])
            EmptyT
            ( NodeT
                (S "D" [Motor 40, Almacen [Comida, Oxigeno, Combustible]] ["Marta", "Sol", "Carmen"])
                EmptyT
                EmptyT
            )
        )
    )

-- sectoresAsignados "Carmen" nave6
-- respuesta --> ["A", "B", "C", "D"]

-- sectoresAsignados "Sofia" nave6
-- respuesta --> ["C"]

-- sectoresAsignados "Maria" nave6
-- respuesta --> []

-- Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]
tripulantes (N ss) = sinRepetidos (tripulantesT ss)

tripulantesT :: Tree Sector -> [Tripulante]
tripulantesT EmptyT = []
tripulantesT (NodeT s si sd) = tripulantesDelSector s ++ tripulantesT si ++ tripulantesT sd

tripulantesDelSector :: Sector -> [Tripulante]
tripulantesDelSector (S _ _ tripulantes) = tripulantes

sinRepetidos :: [Tripulante] -> [Tripulante]
sinRepetidos [] = []
sinRepetidos (t : ts) =
  if t `elem` ts
    then sinRepetidos ts
    else t : sinRepetidos ts

-- tripulantes nave6
-- respuesta --> ["Sofia","Marta","Sol","Carmen"]

-- 4. Manada de lobos

-- Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
-- Los diferentes casos de lobos que forman la jerarquía son los siguientes:
--  - Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
--  - Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres debosques, ríos, etc.), y poseen 2 lobos a cargo.
--  - Las Crias poseen sólo un nombre y no poseen lobos a cargo.

type Presa = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre = String -- nombre de lobo

data Lobo
  = Cazador Nombre [Presa] Lobo Lobo Lobo
  | Explorador Nombre [Territorio] Lobo Lobo
  | Cria Nombre
  deriving (Show)

data Manada = M Lobo
  deriving (Show)

-- 1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean Crias.
-- Resolver las siguientes funciones utilizando recursión estructural sobre la estructura que corresponda en cada caso:

-- Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de Crias.
buenaCaza :: Manada -> Bool
buenaCaza (M lobo) = cantDeCrias lobo < cantDePresas lobo

cantDeCrias :: Lobo -> Int
cantDeCrias (Cria _) = 1
cantDeCrias (Explorador _ _ l1 l2) = cantDeCrias l1 + cantDeCrias l2
cantDeCrias (Cazador _ presas l1 l2 l3) = cantDeCrias l1 + cantDeCrias l2 + cantDeCrias l3

cantDePresas :: Lobo -> Int
cantDePresas (Cria _) = 0
cantDePresas (Explorador _ _ l1 l2) = cantDePresas l1 + cantDePresas l2
cantDePresas (Cazador _ presas l1 l2 l3) = length presas + cantDePresas l1 + cantDePresas l2 + cantDePresas l3

cria1 = Cria "Pancha"

cria2 = Cria "Gordi"

cria3 = Cria "Pocha"

explorador1 = Explorador "Cacho" ["Bosque", "Río"] cria1 cria2

explorador2 = Explorador "Coco" ["Montaña", "Valle"] cria3 cria1

cazador1 = Cazador "Roma" ["Ciervo", "Conejo", "Otra comida", "Antilope", "Jabali", "Conejo", "Conejo"] explorador1 explorador2 cria2

cazador2 = Cazador "Greta" ["Jabalí"] cria3 cria1 explorador1

manada1 :: Manada
manada1 = M cazador1

manada2 :: Manada
manada2 = M cazador2

-- Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
-- con su cantidad de presas. Nota: se considera que los exploradores y Crias tienen cero presas
-- cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
-- cero presas.
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaL lobo

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cria nom) = (nom, 0)
elAlfaL (Explorador nom _ l1 l2) = elegirMaxEntre (nom, 0) (elegirMaxEntre (elAlfaL l1) (elAlfaL l2))
elAlfaL (Cazador nom ps l1 l2 l3) = elegirMaxEntre (nom, cantPresas ps) (elegirMaxEntre (elAlfaL l1) (elegirMaxEntre (elAlfaL l2) (elAlfaL l3)))

elegirMaxEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirMaxEntre (nom1, presas1) (nom2, presas2) =
  if presas1 >= presas2
    then (nom1, presas1)
    else (nom2, presas2)

cantPresas :: [Presa] -> Int
cantPresas ps = length ps

-- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron terr (M l) = losQueExploraronL terr l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cria _) = []
losQueExploraronL t (Cazador _ p l1 l2 l3) = losQueExploraronL t l1 ++ losQueExploraronL t l2 ++ losQueExploraronL t l3
losQueExploraronL t (Explorador nom ts l1 l2) =
  if t `elem` ts
    then nom : losQueExploraronL t l1 ++ losQueExploraronL t l2
    else losQueExploraronL t l1 ++ losQueExploraronL t l2

-- Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
-- dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = exploradoresPorTerritorioL lobo

exploradoresPorTerritorioL :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioL (Cria _) = []
exploradoresPorTerritorioL (Cazador _ _ l1 l2 l3) =
  combinarExploradores
    (exploradoresPorTerritorioL l1)
    (combinarExploradores (exploradoresPorTerritorioL l2) (exploradoresPorTerritorioL l3))
exploradoresPorTerritorioL (Explorador nom ts l1 l2) =
  agregarExploradoresATerritorios
    ts
    nom
    (combinarExploradores (exploradoresPorTerritorioL l1) (exploradoresPorTerritorioL l2))

combinarExploradores :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
combinarExploradores [] lts = lts
combinarExploradores (t : ts) lts =
  agregarExploradoresTerritorio t (combinarExploradores ts lts)

agregarExploradoresTerritorio :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarExploradoresTerritorio (t, n) [] = [(t, n)]
agregarExploradoresTerritorio (t, n) ((ot, ns) : resto) =
  if t == ot
    then (ot, n ++ ns) : resto
    else (ot, ns) : agregarExploradoresTerritorio (t, n) resto

agregarExploradoresATerritorios :: [Territorio] -> Nombre -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarExploradoresATerritorios [] _ lts = lts
agregarExploradoresATerritorios (t : ts) nom lts =
  agregarExploradoresTerritorio (t, [nom]) (agregarExploradoresATerritorios ts nom lts)

-- Propósito: dado el nombre de un lobo y una manada, indica el nombre de todos los cazadores que tienen como subordinado al lobo dado (puede ser un subordinado directo, o el
-- subordinado de un subordinado).
-- Precondición: hay un lobo con dicho nombre y es único.
cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
cazadoresSuperioresDe n (M l) = cazadoresSuperioresDeL n l

cazadoresSuperioresDeL :: Nombre -> Lobo -> [Nombre]
cazadoresSuperioresDeL nombre (Cria _) = []
cazadoresSuperioresDeL nombre (Explorador _ _ l1 l2) = cazadoresSuperioresDeL nombre l1 ++ cazadoresSuperioresDeL nombre l2
cazadoresSuperioresDeL nombre (Cazador n _ l1 l2 l3) =
  if elCazadorEstaEn nombre l1 || elCazadorEstaEn nombre l2 || elCazadorEstaEn nombre l3
    then n : (cazadoresSuperioresDeL nombre l1 ++ cazadoresSuperioresDeL nombre l2 ++ cazadoresSuperioresDeL nombre l3)
    else cazadoresSuperioresDeL nombre l1 ++ cazadoresSuperioresDeL nombre l2 ++ cazadoresSuperioresDeL nombre l3

elCazadorEstaEn :: Nombre -> Lobo -> Bool
elCazadorEstaEn nombre (Cria nom) = nombre == nom
elCazadorEstaEn nombre (Explorador nom _ l1 l2) = nombre == nom || elCazadorEstaEn nombre l1 || elCazadorEstaEn nombre l2
elCazadorEstaEn nombre (Cazador nom _ l1 l2 l3) = nombre == nom || elCazadorEstaEn nombre l1 || elCazadorEstaEn nombre l2 || elCazadorEstaEn nombre l3

-- Suponiendo la siguiente manada de ejemplo:
manadaEj :: Manada
manadaEj =
  M
    ( Cazador
        "DienteFiloso"
        ["Bufalos", "Antilopes"]
        (Cria "Hopito")
        (Explorador "Incansable" ["Oeste hasta el rio"] (Cria "MechonGris") (Cria "Rabito"))
        ( Cazador
            "Garras"
            ["Antílopes", "Ciervos"]
            ( Explorador
                "Zarpado"
                ["Bosque este"]
                (Cria "Osado")
                ( Cazador
                    "Mandibulas"
                    ["Cerdos", "Pavos"]
                    (Cria "Desgreñado")
                    (Cria "Malcriado")
                    ( Cazador
                        "TrituraHuesos"
                        ["Conejos"]
                        (Cria "Peludo")
                        (Cria "Largo")
                        (Cria "Menudo")
                    )
                )
            )
            (Cria "Garrita")
            (Cria "Manchas")
        )
    )

{-
la función cazadoresSuperioresDe debería dar lo siguiente:
cazadoresSuperioresDe "Mandíbulas" manadaEj = ["DienteFiloso", "Garras"]
cazadoresSuperioresDe "Rabito" manadaEj = ["DienteFiloso"]
cazadoresSuperioresDe "DienteFiloso" manadaEj = []
cazadoresSuperioresDe "Peludo" manadaEj = ["DienteFiloso", "Garras", "Mandibulas", "TrituraHuesos"]
-}
