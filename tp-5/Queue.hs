{-
    3. Queue (cola)
    Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa
    que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
    primero en salir (como la cola de un banco).

    3.1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el final de la lista y
    desencolarse por delante.
-}

module Queue (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

data Queue a = Q [a]

-- Crea una cola vacía.
-- O(1)
emptyQ :: Queue a
emptyQ = Q []

-- Dada una cola indica si la cola está vacía.
-- O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ _ = False

-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- O(n) por el costo operacional de enqueue'
enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (enqueue' x xs)

-- O(n) donde n es el largo de la lista
enqueue' :: a -> [a] -> [a]
enqueue' x [] = [x]
enqueue' x (y : ys) = y : enqueue' x ys

-- Dada una cola devuelve el primer elemento de la cola.
-- O(1)
firstQ :: Queue a -> a
-- PARCIAL: la cola no es vacía
firstQ (Q lista) = head lista

-- Dada una cola la devuelve sin su primer elemento.
-- O(1)
dequeue :: Queue a -> Queue a
-- PARCIAL: la cola no es vacía
dequeue (Q lista) = Q (tail lista)
