-- 2. Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
-- la eficiencia entre ambas implementaciones.

module QueueV2 (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

data Queue a = Q [a]

-- Crea una cola vacía.
-- O(1)
emptyQ :: Queue a
emptyQ = Q []

-- Dada una cola indica si la cola está vacía.
-- O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- O(1)
enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (x : xs)

-- Dada una cola devuelve el primer elemento de la cola.
-- O(n) siendo n el coste operacional de firstQ'
firstQ :: Queue a -> a
firstQ (Q lista) = firstQ' lista

-- O(n) siendo n el largo de la lista
firstQ' :: [a] -> a
firstQ' [] = error "No existen elementos en la cola."
firstQ' [x] = x
firstQ' (x : xs) = firstQ' xs

-- Dada una cola la devuelve sin su primer elemento.
-- O(n) siendo n el coste operacional de dequeue'
dequeue :: Queue a -> Queue a
dequeue (Q lista) = Q (dequeue' lista)

-- O(n) siendo n el largo de la lista
dequeue' :: [a] -> [a]
dequeue' [] = error "No existen elementos en la cola"
dequeue' [_] = []
dequeue' (x : xs) = x : dequeue' xs
