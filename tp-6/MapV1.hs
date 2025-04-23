-- Map (diccionario)
-- Como una lista de pares-clave valor sin claves repetidas

module MapV1 (Map, emptyM, assocM, lookupM, deleteM, keys) where

data Map k v = M [(k, v)] -- clave, valor

{-
    INV. REP.: En Map [(clave, valor)]
    - En la lista de pares (clave, valor) que representa el Map, no existen dos pares con la misma clave.
      Es decir, no hay claves repetidas.
-}

mTest :: Map Int String
mTest = M [(1, "uno"), (2, "dos"), (3, "tres"), (4, "cuatro")]

-- assocM 3 "NUEVOVALOR" mTest >>>> respuesta >>>> M [(1,"uno"),(2,"dos"),(3,"NUEVOVALOR"),(4,"cuatro")]
-- assocM 8 "NUEVOVALOR" mTest >>>> respuesta >>>> M [(1,"uno"),(2,"dos"),(3,"tres"),(4,"cuatro"),(8,"NUEVOVALOR")]

-- Propósito: devuelve un map vacío
-- O(1)
emptyM :: Map k v
emptyM = M []

-- Propósito: agrega una asociación clave-valor al map.
assocM :: (Eq k) => k -> v -> Map k v -> Map k v
assocM key value (M kvs) = M (assocM' key value kvs)

assocM' :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
assocM' key value [] = [(key, value)]
assocM' key value (kv : kvs) =
  let keyM = fst kv
   in if keyM == key
        then (keyM, value) : kvs
        else kv : assocM' key value kvs

{- ** Análisis de costos: 
   - assocM' (auxiliar): dada una clave, un valor y una lista de pares clave-valor se realiza una búsqueda sobre dicha lista
                         en el peor caso: la clave no existe en el map y se recorre toda la lista hasta el final 
                        => O(n) siendo n la cantidad de pares clave-valor de la lista

   - assocM (función principal) => O(n) siendo n el costo operacional de assocM'                                              -}

-- Propósito: encuentra un valor dado una clave.
lookupM :: (Eq k) => k -> Map k v -> Maybe v
lookupM key (M kvs) = lookupM' key kvs

lookupM' :: (Eq k) => k -> [(k, v)] -> Maybe v
lookupM' _ [] = Nothing
lookupM' key (kv : kvs) =
  if key == fst kv
    then Just (snd kv)
    else lookupM' key kvs

{- ** Análisis de costos: 
   - lookupM' (auxiliar): Dada una clave k y una lista de pares clave-valor se realiza la recursión en búsqueda de la clave dada 
                          para devolver el valor asociado a dicha clave, en el peor caso no se encuentra dicho valor asociado y 
                          devuelve Nothing. 
                        => O(n) siendo n la cantidad de pares clave-valor de la lista.

   - lookupM (función principal) => O(n) siendo n el costo operacional de lookupM'                                             -}

-- lookupM 3 mTest >>>> respuesta >>>> Just "tres"
-- lookupM 8 mTest >>>> respuesta >>>> Nothing

-- Propósito: borra una asociación dada una clave.
deleteM :: (Eq k) => k -> Map k v -> Map k v
-- PRECOND: La clave a eliminar debe existir en el map.
deleteM key (M kvs) = M (deleteM' key kvs)

deleteM' :: (Eq k) => k -> [(k, v)] -> [(k, v)]
deleteM' key [] = error "La clave a eliminar no existe."
deleteM' key (kv : kvs) =
  let k = fst kv
      v = snd kv
   in if k == key
        then kvs
        else (k, v) : deleteM' key kvs

{- ** Análisis de costos: 
   - deleteM' (auxiliar): Dada una clave k y una lista de pares clave-valor se realiza la recursión en búsqueda de la clave dada 
                          para eliminar el par asociado del map. En el peor caso se recorre toda la lista y el par no existe.
                        => O(n) siendo n la cantidad de pares clave-valor de la lista.

   - deleteM (función principal) => O(n) siendo n el costo operacional de deleteM'                                             -}

-- Propósito: devuelve las claves del map
keys :: Map k v -> [k]
keys (M kvs) = keys' kvs

keys' :: [(k, v)] -> [k]
keys' [] = []
keys' (kv : kvs) = fst kv : keys' kvs

{- ** Análisis de costos: 
   - keys' (auxiliar): Dada una lista de pares clave-valor se realiza la recursión acumulando en una lista todas las claves
                       existentes de dicha lista 
                     => O(n) siendo n la cantidad de pares clave-valor de la lista.

   - keys (función principal) => O(n) siendo n el costo operacional de keys'                                                  -}