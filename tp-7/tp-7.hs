import Data.Maybe (fromJust)
import MapV1 (Map, assocM, emptyM, keys, lookupM)

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- Propósito: dado un BST dice si el elemento pertenece o no al árbol.
-- Costo: O(log N)
belongsBST :: (Ord a) => a -> Tree a -> Bool
belongsBST _ EmptyT = False
belongsBST x (NodeT y ti td) =
  (x == y) || belongsBST x (seguirPor x y ti td)

seguirPor :: (Ord a) => a -> a -> Tree a -> Tree a -> Tree a
seguirPor x y ti td =
  if x < y
    then ti
    else td

{- Justificación de costos:
   - N es el total de elementos en el arbol h es la altura de un arbol (la rama mas larga) => h = O(log N)
   - El costo de belongsBST es O(log N) en el peor caso se recorre una rama.
   - Esto se debe a que, en cada paso, la función compara el elemento buscado con el nodo actual y descarta una de las ramas (izquierda o derecha),
     reduciendo el problema a la mitad en cada llamado recursivo.
-}
-- Propósito: dado un BST inserta un elemento en el árbol.
-- Costo: O(log N)
insertBST :: (Ord a) => a -> Tree a -> Tree a
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) =
  if x < y
    then NodeT y (insertBST x ti) td
    else
      if x > y
        then NodeT y ti (insertBST x td)
        else NodeT x ti td

bst :: Tree Int
bst = NodeT 8 (NodeT 3 (NodeT 1 EmptyT EmptyT) (NodeT 6 (NodeT 4 EmptyT EmptyT) (NodeT 7 EmptyT EmptyT))) (NodeT 10 EmptyT (NodeT 14 (NodeT 13 EmptyT EmptyT) EmptyT))

{-
                8
              /     \
            3         10
          /   \        \
        1     6        14
              / \      /
            4   7    13

  si quiero insertar 15 =
                8
              /     \
            3         10
          /   \        \
        1     6        14
              / \      /  \
            4   7    13  15

  >> insertBST 2 bst
    respuesta: NodeT 8 (NodeT 3 (NodeT 1 EmptyT (NodeT 2 EmptyT EmptyT)) (NodeT 6 (NodeT 4 EmptyT EmptyT) (NodeT 7 EmptyT EmptyT))) (NodeT 10 EmptyT (NodeT 14 (NodeT 13 EmptyT EmptyT) EmptyT))
-}

{-
 >> Dada la siguiente interfaz y costos para el tipo abstracto Map:

  *  emptyM :: Map k v                               --Costo: O(1).
  *  assocM :: Ord k => k -> v -> Map k v -> Map k v --Costo: O(log K).
  *  lookupM :: Ord k => k -> Map k v -> Maybe v     --Costo: O(log K).
  *  deleteM :: Ord k => k -> Map k v -> Map k v     --Costo: O(log K).
  *  keys :: Map k v -> [k]                          -- Costo: O(K).

 - Recalcular el costo de las funciones como usuario de Map de la práctica anterior, siendo K es la cantidad de claves del Map.
   Justficar las respuestas.
-}

-- Propósito: obtiene los valores asociados a cada clave del map.
-- Costo:  O(K log KK)
valuesM :: (Eq k) => Map k v -> [Maybe v]
valuesM map = valuesM' (keys map) map

valuesM' :: (Eq k) => [k] -> Map k v -> [Maybe v]
valuesM' [] map = []
valuesM' (k : ks) map = lookupM k map : valuesM' ks map

{- Justificación de costos valuesM: sea k la cantidad total de claves del map
   * valuesM realiza un recorrido sobre la lista pasada como argumento [k] -> m pasos -> m = K
     -> realiza K llamados con un costo de O(log K) -> K * O(log K)
     -> el costo total de 'valuesM'' es O(K log K)
   * la función principal realiza 'valuesM'' con costo O(K log K) y 'keys' que trabaja con el mismo mapa con un costo de O(k)
     -> Estas operaciónes son secuenciales e independientes, por lo tanto, el costo sería O(K log K) + O(K)
        la función auxiliar valuesM' crece más rapidamente con respecto a K, entonces O(K) de keys se absorbe en el análisis final.
   => El costo final de valuesM es: O(K log K)
-}

-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: (Eq k) => [k] -> Map k v -> Bool
todasAsociadas [] map = True
todasAsociadas (k : ks) map = k `elem` keys map && todasAsociadas ks map

{- Justificación de costos todasAsociadas: * sea m la cantidad de claves en la lista [k]
                                           * sea k la cantidad total de claves del map
   * la función realiza un recorrido sobre la lista pasada como argumento [k] -> m pasos (peor caso)
   * la función 'keys' tiene un costo O(K) - basándo su costo en el total de claves del map y la función 'elem' tiene un costo de O(K) siendo K
     el total de claves de 'keys map'
     -> Estas operaciones son secuenciales e independientes, por lo tanto el costo de realizar estas dos operaciones es de
        O(K) + K = O(K) ->> por lo tanto, el costo de cada paso es de O(K)
   * En total, en el PEOR CASO se realizan m pasos con un costo de O(K) cada uno -> m * O(K)
   => El costo final de todasAsociadas es: O(m*K)
-}

-- Propósito: convierte una lista de pares clave valor en un map.
listToMap :: (Eq k) => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap (kv : kvs) = assocM (fst kv) (snd kv) (listToMap kvs)

{- Justificación de costos listToMap: * sea m la cantidad de elementos de la lista de pares [(k, v)]
                                      * sea K la cantidad de claves distintas de un map y K <= m
   * la función realiza un recorrido sobre la lista pasada como argumento
   * se realiza un assocM con un costo de O(log K) siendo K la cantidad de claves del 'nuevo map' que se  va armando en cada paso de la recursión.
     -> Entonces, cada paso de la recursión tiene un costo de O(log k) y esto se realiza m veces
   * El costo final de listToMap es: O(m log K) -> donde K es el tamaño final del map =>
   => PERO en el peor caso K = m, y el costo final es O(m log m)
-}

-- Propósito: convierte un map en una lista de pares clave valor.
mapToList :: (Eq k) => Map k v -> [(k, v)]
mapToList map = mapToList' (keys map) map

mapToList' :: (Eq k) => [k] -> Map k v -> [(k, v)]
mapToList' [] map = []
mapToList' (k : ks) map = (k, fromJust (lookupM k map)) : mapToList' ks map

{- Justificación de costos mapToList: * sea K la cantidad de claves del map
   * en la función auxiliar mapToList' se realiza un lookupM de costo O(log K) con un fromJust O(1) y esto se realiza con todas las claves distintas
     del map => (O(log K) + O(1) * K) = O(log K) * K = O(K log K)
   * En la función principal se realiza keys map con un costo de O(K) y la auxiliar mapToList'
     -> Estas operaciones son secuenciales e independientes, por lo tanto su costo es de O(K log K) + K
   => El costo final de mapToList es: O(K log K) + K = O(K log K) (el costo de keys es absorbido por ser la funcion que tiene menor impacto dependiedo de K)
-}

-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: (Eq k) => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k, v) : kvs) =
  let mapRec = agruparEq kvs
   in case lookupM k mapRec of
        Nothing -> assocM k [v] mapRec
        Just vs -> assocM k (v : vs) mapRec

{- Justificación de costos agruparEq: * sea m la cantidad de asociaciones clave-valor de la lista dada por parámetro
                                      * sea k la cantidad total de claves distintas en un map
                                      * K ≤ m
   * Se recorre la lista de pares O(m) y en cada paso se realiza un lookupM O(log k) y un assoc O(log K)
     esto significa que cada paso cuesta O(log k) + O(log k) estas operaciones son secuenciales e independientes, quedando con un costo de O(log k)
   * Se realizan m pasos, ya que m podria ser mayor o igual a k
   => El costo final es de O(m * log k) en peor caso si k = m => O(m log m)
-}

-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número
--            asociado con dichas claves.
incrementar :: (Eq k) => [k] -> Map k Int -> Map k Int
incrementar [] map = map
incrementar (k : ks) map = case lookupM k map of
  Just v -> assocM k (v + 1) (incrementar ks map)
  Nothing -> incrementar ks map

{- Justificación de costos incrementar: * sea m la cantidad de claves de la lista dada como argumento
                                        * sea k la cantidad total de claves distintas del map
   * Se realiza un recorrido sobre la lista (m pasos), cada paso realiza un lookupM (O(log k))en busca del valor y un assocM (O(log k))
   * Entonces el costo de cada paso es de: O(log k) + O(log k) = O(log k)
  => Entonces el costo final de esta operación es de O(m log k) se realizan m pasos con un costo de log k cada uno
-}

-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo.
-- Si una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps :: (Eq k) => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = mergeMaps' (keys map1) map1 map2

mergeMaps' :: (Eq k) => [k] -> Map k v -> Map k v -> Map k v
mergeMaps' [] _ map2 = map2
mergeMaps' (k : ks) map1 map2 = assocM k (fromJust (lookupM k map1)) (mergeMaps' ks map1 map2)

{- Justificación de costos mergeMaps: * sea k la cantidad de claves que tiene el map1 pasado como argumento
                                      * sea k' la cantidad de claves distintas que tiene el map2 pasado como argumento

   * la función auxiliar mergeMaps' realiza una recursión sobre una lista de claves m pasada como argumento
   * en cada paso realiza un lookupM sobre map1 con costo O(log k) y un assocM sobre map2
   * el costo de assocM depende del tamaño del map sobre el que se inserta, que en este caso crece desde tamaño k'
     hasta un máximo de k + k' -> porque en el peor caso seria que ninguna de las claves de map1 está presente en map2,
     por lo tanto, cada assocM agregaria una nueva clave distinta.
   * Entonces, el costo de cada paso es O(log k + log k') y se repite m veces (donde m = k, cantidad de claves de map1).
   * Por lo tanto, el costo total de la función auxiliar mergeMaps' es O(k * (log k + log k'))

   * la función principal realiza mergeMaps' O(k * (log k + log k')) y keys map1 (O(k)).
  => Entonces, el costo final de mergeMaps es: O(k * (log k + log k')) + O(k) = O(k * (log k + log k'))
     el costo de keys se desestima ya que tiene menor impacto con respecto k
-}
