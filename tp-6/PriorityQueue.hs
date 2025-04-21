{-
  1. Priority Queue (cola de prioridad)

  La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
  posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.

  Implementarla usando listas, e indicando el costo de cada operación.
-}

module PriorityQueue (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ) where

data PriorityQueue a = PQ [a]

{-
  INV. REP.: En PQ xs
  - xs está ordenada de menor a mayor nivel de prioridad
-}

-- Propósito: devuelve una priority queue vacía.
-- O(1)
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

-- Propósito: indica si la priority queue está vacía.
-- O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

-- Propósito: inserta un elemento en la priority queue.
insertPQ :: (Ord a) => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ xs) = PQ (insertPQ' x xs)

-- O(1)
insertPQ' :: (Ord a) => a -> [a] -> [a]
insertPQ' x [] = [x]
insertPQ' x (y : ys) =
  if x <= y
    then x : y : ys
    else y : insertPQ' x ys

-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
-- O(1)
findMinPQ :: (Ord a) => PriorityQueue a -> a
findMinPQ (PQ xs) = head xs

-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.
-- O(1)
deleteMinPQ :: (Ord a) => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (tail xs)