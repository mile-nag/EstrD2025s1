-- Map (diccionaro)
-- Como una lista de paires-clave valor con claves repetidas

module MapV2 (Map, emptyM, assocM, lookupM, deleteM, keys) where

data Map k v = M [(k, v)] -- clave, valor
{-
    INV. REP.: En Map [(clave, valor)]
    - La lista puede contener múltiples asociaciones con la misma clave.
    - La asociación más reciente (la primera en la lista) es la que se devuelve al hacer lookup o delete.
-}

-- Propósito: devuelve un map vacío
-- O(1)
emptyM :: Map k v
emptyM = M []

--O(1)
-- Propósito: agrega una asociación clave-valor al map.
assocM :: (Eq k) => k -> v -> Map k v -> Map k v
assocM key value (M kvs) = M ((key,value) : kvs)  

-- Propósito: encuentra un valor dado una clave.
-- O(n)
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

-- Propósito: borra una asociación dada una clave.
-- O(n) 
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
-- O(n)
keys :: Map k v -> [k]
keys (M kvs) = keys' kvs

keys' :: [(k, v)] -> [k]
keys' [] = []
keys' (kv : kvs) = fst kv : keys' kvs

{- ** Análisis de costos: 
   - keys' (auxiliar): Dada una lista de pares clave-valor se realiza la recursión acumulando en una lista todas las claves
                       existentes de dicha lista 
                     => O(n) siendo n la cantidad de pares clave-valor de la lista.

   - keys (función principal) => O(n) siendo n el costo operacional de keys' -}

mTest :: Map Int String
mTest = M [(1, "uno"), (2, "dos"), (3, "tres"), (4, "cuatro")]

-- assocM 3 "NUEVOVALOR" mTest >>>> respuesta >>>> M [(1,"uno"),(2,"dos"),(3,"NUEVOVALOR"),(4,"cuatro")]
-- assocM 8 "NUEVOVALOR" mTest >>>> respuesta >>>> M [(1,"uno"),(2,"dos"),(3,"tres"),(4,"cuatro"),(8,"NUEVOVALOR")]

-- lookupM 3 mTest >>>> respuesta >>>> Just "tres"
-- lookupM 8 mTest >>>> respuesta >>>> Nothing