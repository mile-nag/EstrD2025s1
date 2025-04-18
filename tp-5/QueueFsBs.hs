module QueueFsBs (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

data Queue a = Q [a] [a] --   fs  bs

{-
    - fs representa a los elementos que se encolan (parte de entrada)
    - bs representa a los elementos que se desencolan (parte de salida)
-}

-- Crea una cola vacía.
-- O(1)
emptyQ :: Queue a
emptyQ = Q [] []

-- Indica si la cola está vacía
-- O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q [] []) = True
isEmptyQ _ = False

-- Agrega un elemento al final
-- O(1)
enqueue :: a -> Queue a -> Queue a
enqueue x (Q fs bs) = Q (x : fs) bs

-- Devuelve el primer elemento
-- O(1) amortizado
firstQ :: Queue a -> a
firstQ (Q fs bs) = head (actualizarCola fs bs)

-- Quita el primer elemento
-- O(1) amortizado
dequeue :: Queue a -> Queue a
dequeue (Q fs bs) =
  let nuevos = actualizarCola fs bs
   in Q [] (tail nuevos)

-- Si la parte de salida está vacía, revierte la de entrada. Si no, la devuelve tal como está.
-- O(n)
actualizarCola :: [a] -> [a] -> [a]
actualizarCola fs [] = reverse fs
actualizarCola _ bs = bs

{-

El costo de firstQ y dequeue es O(1), solo hace reverse con (actualizarCola )costo O(n) en caso de que bs esté vacía,
y aunque este es el peor caso está amortizado por los otros n casos que se realizan.

Entonces, el costo si es de O(n) PERO esta amortizado porque es 1 caso cada n casos, entonces el costo sería O(1) amortizado:

El costo final de firstQ es:
\** O(1) si bs no está vacía se accede directamente con head
\** O(n) solo si bs está vacía, se invierte fs una vez con reverse fs
\** Ese costo O(n) se amortiza entre las siguientes n operaciones --> costo final O(1) amortizado

El costo final de dequeue es:
\** O(1) normalmente porque solo usa tail
\** O(n) solo cuando bs está vacío (reverse)
\** Pero ese O(n) se amortiza en las siguientes n operaciones --> costo final O(1) amortizado

-}
