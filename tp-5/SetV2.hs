{-
 3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En otras palabras, al agregar no va a chequear que si el elemento    ya se encuentra en la lista, pero sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,por ejemplo
 Contrastar la eficiencia obtenida en esta implementación con la anterior.
-}

module SetV2 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

data Set a = S [a]

{- INV. REP.:
  - no hay repetidos en la lista para el usuario
-}

-- Crea un conjunto vacío.
-- O(1)
emptyS :: Set a
emptyS = S []

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- O(1)
addS :: (Eq a) => a -> Set a -> Set a
addS x (S xs) = S (x : xs)

-- Aux que elimina repetidos de una lista
-- O(n^2) -- cuadrática: realiza una operacion lineal por cada elemento de la lista (n)
sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x : xs) =
  if x `elem` xs
    then sinRepetidos xs
    else x : sinRepetidos xs

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- O(n) siendo n la cantidad de elementos del set
belongs :: (Eq a) => a -> Set a -> Bool
belongs x (S xs) = x `elem` xs

-- Devuelve la cantidad de elementos distintos de un conjunto.
--       O(n^2) + O(n) = O(n^2)           -- el costo operacional de length es absorbido por el dominante (sinRepetidos)
-- sinRepetidos + length
sizeS :: (Eq a) => Set a -> Int
sizeS (S xs) = length (sinRepetidos xs)

-- Borra un elemento del conjunto.
-- O(n) siendo n el coste operacional de 'eliminar'
removeS :: (Eq a) => a -> Set a -> Set a
removeS x (S xs) = S (eliminarTodos x xs)

-- O(n) siendo n la cantidad de elementos de la lista, ya que en el peor caso se recorre entera
eliminarTodos :: (Eq a) => a -> [a] -> [a]
eliminarTodos _ [] = []
eliminarTodos x (y : ys) =
  if x == y
    then eliminarTodos x ys
    else y : eliminarTodos x ys

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
-- O(n) siendo n el coste operacional de ++
unionS :: (Eq a) => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (xs ++ ys)

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-- O(n^2) + O(1) = O(n^2) -- cuadrática
setToList :: (Eq a) => Set a -> [a]
setToList (S xs) = sinRepetidos xs
