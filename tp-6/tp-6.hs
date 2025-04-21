{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}

import Map (Map, assocM, deleteM, emptyM, keys, lookupM)
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

m1 :: Map Int String
m1 = assocM 1 "uno" (assocM 2 "dos" (assocM 3 "tres" (assocM 4 "cuatro" (assocM 5 "cinco" emptyM))))

{-
-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool

-- Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v

-- Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]

-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v]

-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int

-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v

-}
