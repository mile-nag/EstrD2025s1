-- Set (conjunto)
-- Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda la cantidad de elementos en la estructura.

module Set (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

data Set a = Set [a] Int -- Int: cantidad de elementos en el set

{- INV. REP.:
    - no hay repetidos en la lista
    - n indica la cantidad de elementos de la lista xs
-}

-- Crea un conjunto vacío.
-- O(1)
emptyS :: Set a
emptyS = Set [] 0

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- O(n) siendo n el costo operacional de elem que realiza una búsqueda sobre la lista dada -- lineal
addS :: (Eq a) => a -> Set a -> Set a
addS x (Set xs n) =
  if x `elem` xs
    then Set xs n
    else Set (x : xs) (n + 1)

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- O(n) siendo n la cantidad de elementos del set
belongs :: (Eq a) => a -> Set a -> Bool
belongs x (Set elems _) = x `elem` elems

-- Devuelve la cantidad de elementos distintos de un conjunto.
-- O(1)
sizeS :: (Eq a) => Set a -> Int
sizeS (Set _ n) = n

-- Borra un elemento del conjunto.
-- O(n) siendo n el coste operacional de 'eliminar'
removeS :: (Eq a) => a -> Set a -> Set a
removeS x (Set xs n) = Set (eliminar x xs) (n - 1)

-- O(n) siendo n la cantidad de elementos de la lista, ya que en el peor caso se recorre entera
eliminar :: (Eq a) => a -> [a] -> [a]
eliminar x [] = error "El elemento a eliminar no existe."
eliminar x (y : ys) =
  if x == y
    then ys
    else y : eliminar x ys

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
-- O(n) siendo n el coste operacional de ++
unionS :: (Eq a) => Set a -> Set a -> Set a
unionS (Set xs n) (Set ys m) = Set (xs ++ ys) (n + m)

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-- O(1)
setToList :: (Eq a) => Set a -> [a]
setToList (Set xs n) = xs
