{-
2. Map (diccionario)
Ejercicio 3
La interfaz del tipo abstracto Map es la siguiente:

-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use foldr" #-}
module Map (Map, emptyM, assocM, lookupM, deleteM, keys) where

data Map k v = M [(k, v)]
  --                clave, valor
  deriving (Show)

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

-- Propósito: encuentra un valor dado una clave.
lookupM :: (Eq k) => k -> Map k v -> Maybe v
lookupM key (M kvs) = lookupM' key kvs

lookupM' :: (Eq k) => k -> [(k, v)] -> Maybe v
lookupM' _ [] = Nothing
lookupM' key (kv : kvs) =
  if key == fst kv
    then Just (snd kv)
    else lookupM' key kvs

-- lookupM 3 mTest >>>> respuesta >>>> Just "tres"
-- lookupM 8 mTest >>>> respuesta >>>> Nothing

-- Propósito: borra una asociación dada una clave.
deleteM :: (Eq k) => k -> Map k v -> Map k v
deleteM key (M kvs) = M (deleteM' key kvs)

deleteM' :: (Eq k) => k -> [(k, v)] -> [(k, v)]
deleteM' key [] = []
deleteM' key (kv : kvs) =
  let k = fst kv
      v = snd kv
   in if k == key
        then kvs
        else (k, v) : deleteM' key kvs

-- Propósito: devuelve las claves del map
keys :: Map k v -> [k]
keys (M kvs) = keys' kvs

keys' :: [(k, v)] -> [k]
keys' [] = undefined
keys' (kv : kvs) = fst kv : keys' kvs
