-- MultiSet (multiconjunto)

module MultiSet (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList) where

import Data.Maybe (fromJust, fromMaybe)
import Distribution.Compat.Prelude (isNothing)
import MapV1 (Map, assocM, deleteM, emptyM, keys, lookupM)

data MultiSet a = MS (Map a Int) -- MS (M [(elemento, apariciones del elemento)])

{- INV. REP.: En MS (Map e ocurrencias)
   - Todos los valores en ocurrencias en el Map deben ser mayores a 0. Es decir, no debe haber entradas con valores cero o negativos.
-}

-- Propósito: denota un multiconjunto vacío.
-- O(1)
emptyMS :: MultiSet a
emptyMS = MS emptyM

-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS :: (Ord a) => a -> MultiSet a -> MultiSet a
addMS x (MS m) =
  let mv = lookupM x m
   in if isNothing mv
        then MS (assocM x 1 m)
        else MS (assocM x (fromJust mv + 1) m)

-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
ocurrencesMS :: (Ord a) => a -> MultiSet a -> Int
ocurrencesMS x (MS m) = undefined

---- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
unionMS :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MS m1) (MS m2) = MS (unionMS' (mapToList m1) m2)

unionMS' :: (Ord a) => [(a, Int)] -> Map a Int -> Map a Int
unionMS' [] m = m
unionMS' (ki : kis) m =
  let k = fst ki
      i = snd ki
      lk = lookupM k m
   in if isNothing lk
        then assocM k i (unionMS' kis m)
        else assocM k (i + fromJust lk) (unionMS' kis m)

-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.
intersectionMS :: (Ord a) => MultiSet a -> MultiSet a -> MultiSet a
intersectionMS (MS m1) (MS m2) = MS (intersectionMS' (mapToList m1) m2)

intersectionMS' :: (Ord a) => [(a, Int)] -> Map a Int -> Map a Int
intersectionMS' [] m = emptyM
intersectionMS' (ki : kis) m =
  let k = fst ki
      i = snd ki
      lk = lookupM k m
   in if isNothing lk
        then intersectionMS' kis m
        else assocM k (i + fromJust lk) (intersectionMS' kis m)

-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.
multiSetToList :: (Eq a) => MultiSet a -> [(a, Int)]
multiSetToList (MS m) = mapToList m

-- (arch. tp-6 - Función como usuario del tipo map) Propósito: convierte un map en una lista de pares clave valor.
mapToList :: (Eq k) => Map k v -> [(k, v)]
mapToList map = mapToList' (keys map) map

mapToList' :: (Eq k) => [k] -> Map k v -> [(k, v)]
mapToList' [] map = []
mapToList' (k : ks) map = (k, fromJust (lookupM k map)) : mapToList' ks map
