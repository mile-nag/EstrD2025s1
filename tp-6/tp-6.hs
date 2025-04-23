{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use uncurry" #-}

import Data.Maybe (fromJust)
import MapV1 (Map, assocM, deleteM, emptyM, keys, lookupM)
import PriorityQueue (PriorityQueue, deleteMinPQ, emptyPQ, findMinPQ, insertPQ, isEmptyPQ)

{-
    >> PriorityQueue:
    *  emptyPQ     :: PriorityQueue a
    *  isEmptyPQ   :: PriorityQueue a -> Bool
    *  insertPQ    :: Ord a => a -> PriorityQueue a -> PriorityQueue a
    *  findMinPQ   :: Ord a => PriorityQueue a -> a                             -- Precondición: no vacía
    *  deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a               -- Precondición: no vacía

-}
-- Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar. ¿Cuál es su costo?

-- Dada una lista la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar
-- construirPQ O(n^2)
-- obtenerTodos O(n)
-- heapSort utiliza las dos operaciones, se realizan una despues de la otra y no dependen entre sí
-- El costo final de esta operacion es: O(n^2 + n) = O(n^2) ya que el n es absorbido por el mayor costo.

heapSort :: (Ord a) => [a] -> [a]
heapSort xs =
  let pq = construirPQ xs
   in obtenerTodos pq

-- emptyPQ O(1)
-- insertPQ es O(n) en el peor caso, donde k es el tamaño actual de la PQ
-- para n elementos: O(n^2) -- se realiza una operacion lineal por cada elemento en la pq
construirPQ :: (Ord a) => [a] -> PriorityQueue a
construirPQ [] = emptyPQ
construirPQ (x : xs) = insertPQ x (construirPQ xs)

-- isEmptyPQ O(1)
-- findMinPQ O(1)
-- deleteMinPQ O(1)
-- Estas operaciones se realizan n veces (cantidad de elementos de la pq)
-- Costo de obtenerTodos: O(n) - constante: cada paso es O(1) y se realiza n veces
obtenerTodos :: (Ord a) => PriorityQueue a -> [a]
obtenerTodos pq =
  if isEmptyPQ pq
    then []
    else
      let m = findMinPQ pq
          pq' = deleteMinPQ pq
       in m : obtenerTodos pq'

-- ---------------------------------------------------------------------------------------------------------------------------

{-
    >> Map
    emptyM :: Map k v
    assocM :: Eq k => k -> v -> Map k v -> Map k v
    lookupM :: Eq k => k -> Map k v -> Maybe v
    deleteM :: Eq k => k -> Map k v -> Map k v
    keys :: Map k v -> [k]
-}

-- Indicar los ordenes de complejidad en peor caso de cada función implementada, justificando las respuestas.

-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: (Eq k) => Map k v -> [Maybe v]
valuesM map = valuesM' (keys map) map

valuesM' :: (Eq k) => [k] -> Map k v -> [Maybe v]
valuesM' [] map = []
valuesM' (k : ks) map = lookupM k map : valuesM' ks map

{- Análisis de costos valuesM:
 * La función keys recorre toda la lista interna del map para obtener las claves, por lo que tiene costo O(m), donde m es la cantidad de
   pares clave-valor del map
 * La función auxiliar valuesM' recorre la lista de claves (la lista [k] tiene la misma cantidad de elementos que el map -> m elementos)
   y por cada una realiza una operación lookupM, que también tiene costo O(m) en una representación como lista de pares
 * Por lo tanto, el costo total es O(m^2): se hacen m llamadas (una por cada clave) y cada llamada cuesta O(m)                            -}

-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: (Eq k) => [k] -> Map k v -> Bool
todasAsociadas [] map = True
todasAsociadas (k : ks) map = k `elem` keys map && todasAsociadas ks map

{- Análisis de costos todasAsociadas:
 * La función keys tiene un costo O(m), siendo m la cantidad de elementos del map
 * La función elem también tiene costo O(m), ya que recorre la lista devuelta por keys
 * Cada llamada realiza primero keys (O(m)) y luego elem (O(m)), por lo tanto el costo total por paso es O(m) + O(m) = O(m).
 * Por lo tanto, el costo total es de O(n * m): se realizan n llamados (una por cada clave de la lista) y cada llamada cuesta O(m)        -}

-- Propósito: convierte una lista de pares clave valor en un map.
listToMap :: (Eq k) => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap (kv : kvs) = assocM (fst kv) (snd kv) (listToMap kvs)

{- Análisis de costos listToMap:
 * La función emptyM tiene costo O(1) -> se desestima
 * fst kv y snd kv tienen costo O(1)  -> se desestima
 * La función assocM tiene costo O(m), siendo m la cantidad de elementos en el map actual (que se construye recursivamente utilizando la
   lista de pares clave-valor pasada como argumento).
 * En cada paso recursivo se llama a assocM sobre un map de tamaño creciente (desde 0 hasta n−1), siendo n la cantidad de pares en la lista.
 * Por lo tanto, el costo total es la suma:
    - en la 1° llamada se ejecuta assocM sobre un map de tamaño 0 → costo O(0)
    - en la 2° sobre uno de tamaño 1 -> costo O(1)
    - en la 3° sobre uno de tamaño 2 -> costo O(2)
    - ...
    - en la n-ésima sobre un map de tamaño n-1 -> costo O(n−1)
    => Entonces: O(0 + 1 + 2 + ... + n−1) = O(n²).                                                                                         -}

-- Propósito: convierte un map en una lista de pares clave valor.
mapToList :: (Eq k) => Map k v -> [(k, v)]
mapToList map = mapToList' (keys map) map

mapToList' :: (Eq k) => [k] -> Map k v -> [(k, v)]
mapToList' [] map = []
mapToList' (k : ks) map = (k, fromJust (lookupM k map)) : mapToList' ks map

{- Análisis de costos mapToList:
 * La función keys tiene un costo O(m), siendo m la cantidad de elementos del map
 * En base a ese mismo m se genera la lista [k] (con el llamado a keys) que es la que se utiiza en mapToList' para recorrer.
 * Cada llamado realiza:
     - un llamado a fromJust (costo O(1), se desestima)
     - un llamado a lookupM (costo O(m), siendo m el tamaño del map)
 * Como se hacen m llamados recursivos (uno por cada clave), y cada uno cuesta O(m), el costo total es O(m * m) = O(m²).                    -}

-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: (Eq k) => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k, v) : kvs) =
  let mapRec = agruparEq kvs
   in case lookupM k mapRec of
        Nothing -> assocM k [v] mapRec
        Just vs -> assocM k (v : vs) mapRec

{- Análisis de costos agruparEq:
 * La función recorre todos los elementos de la lista, o sea n pasos.
 * El Map comienza vacío y llega a tener como máximo m claves distintas (m ≤ n).
 * Como assocM y lookupM tienen costo lineal en el tamaño del Map (por su representación como lista), el costo de cada paso es proporcional
   al tamaño del Map en ese momento.
 * Por lo tanto, el costo total es la suma:
    - en la 1° llamada se ejecuta assocM sobre un map de tamaño 0 → costo O(0)
    - en la 2° sobre uno de tamaño 1 -> costo O(1)
    - en la 3° sobre uno de tamaño 2 -> costo O(2)
    - ...
    - en la n-ésima sobre un map de tamaño n-1 -> costo O(n−1)
    => Costo total: O(0 + 1 + 2 + ... + n−1) = O(n²) en el peor caso.                                                                                       -}

-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número
--            asociado con dichas claves.
incrementar :: (Eq k) => [k] -> Map k Int -> Map k Int
incrementar [] map = map
incrementar (k : ks) map = case lookupM k map of
  Just v -> assocM k (v + 1) (incrementar ks map)
  Nothing -> incrementar ks map 

-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo.
-- Si una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps :: (Eq k) => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = mergeMaps' (keys map1) map1 map2

mergeMaps' :: (Eq k) => [k] -> Map k v -> Map k v -> Map k v
mergeMaps' [] _ map2 = map2 
mergeMaps' (k : ks) map1 map2 = mergeMaps' ks map1 (assocM k (fromJust (lookupM k map1)) map2)

{- Análisis de costos mergeMaps:
 * La función principal realiza un keys con un costo O(m) siendo m la cantidad de elementos del map1
 * En la función auxiliar se realiza un llamado m veces, siendo m la cantidad de elementos del map1
   (es la lista que viene de keys map1)
 * En cada llamado se realiza un llamado a assocM con un costo de O(n) siendo n la cantidad de elemntos en el map2
   y un lookup que cuesta O(m) siendo m la cantidad de elementos del map1
   -> cada llamado cuesta O (n^2 + n*m ) (es lo mismo que O(n * (n + m)))
 * El costo final de la función principal sería O((n^2 + n*m) + n),
   sabemos que el costo de keys es el mas chico entonces se desestima
 => El costo final de esta operación es de:  O(n^2 + n*m)                                                             -}

m1 :: Map Int String
m1 = assocM 1 "uno" (assocM 2 "dos" (assocM 3 "tres" (assocM 4 "cuatro" (assocM 5 "cinco" emptyM))))

lk :: [String]
lk = ["a", "b", "c", "d"]

m3 :: Map String Int
m3 = assocM "a" 1 (assocM "b" 1 (assocM "c" 1 (assocM "d" 1 emptyM)))

m4 :: Map String Int
m4 = assocM "g" 1 (assocM "h" 1 (assocM "c" 1 (assocM "d" 1 (assocM "a" 1 emptyM))))

m5 :: Map String Int
m5 = assocM "a" 2 (assocM "b" 2 (assocM "c" 2 (assocM "d" 2 emptyM)))

lkv :: [(String, String)]
lkv = [("a", "aa"), ("b", "bb"), ("c", "cc"), ("d", "dd"), ("e", "ee"), ("f", "ff"), ("g", "gg")]

-- >> valuesM m1
-- respuesta >> [Just "cinco",Just "cuatro",Just "tres",Just "dos",Just "uno"]

-- >> todasAsociadas [1,2,3,4,5,6,7] m1
-- respuesta >> False

-- >> todasAsociadas [1,3,4,5] m1
-- respuesta >> True

-- >> listToMap lkv
-- respuesta >> M [("g","gg"),("f","ff"),("e","ee"),("d","dd"),("c","cc"),("b","bb"),("a","aa")]

-- >> mapToList m1
-- respuesta >> [(5,"cinco"),(4,"cuatro"),(3,"tres"),(2,"dos"),(1,"uno")]

-- >> agruparEq [("a", 1), ("b", 2), ("a", 3), ("b", 4), ("c", 5)]
-- respuesta >> M [("c",[5]),("b",[2,4]),("a",[1,3])]

-- >> incrementar lk m3
-- respuesta >> M [("d",2),("c",2),("b",2),("a",2)]

-- >> incrementar lk m4
-- respuesta >> M [("d",2),("c",2),("a",2)]

-- Ejercicio 5:

-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista.
indexar :: [a] -> Map Int a
indexar xs = indexar' xs 0 emptyM

indexar' :: [a] -> Int -> Map Int a -> Map Int a
indexar' [] _ map = map
indexar' (x : xs) i map = indexar' xs (i + 1) (assocM i x map)

{- ** Justificación de costos: indexar
  - assocM tiene costo O(n), donde n es la cantidad de claves en el Map actual
  - Se llama n veces (una por cada elemento de la lista)
  - Entonces el costo total es: O(1 + 2 + 3 + ... + n) = O(n²)
-}

-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> Map Char Int
ocurrencias str = ocurrencias' str emptyM

ocurrencias' :: String -> Map Char Int -> Map Char Int
ocurrencias' [] map = map
ocurrencias' (c : cs) map =
  case lookupM c map of
    Just v -> ocurrencias' cs (assocM c (v + 1) map)
    Nothing -> ocurrencias' cs (assocM c 1 map)

{- ** Justificación de costos: ocurrencias 
  - Por cada carácter:
   - lookupM -> O(m), siendo m la cantidad de claves en ese momento.
   - assocM  también O(m).
   - Entonces, cada paso cuesta O(m), y en el peor caso m puede crecer hasta n (todos los caracteres son distintos).
-}

m6 :: Map String Int
m6 = assocM "a" 2 (assocM "b" 2 (assocM "c" 2 (assocM "d" 2 emptyM)))