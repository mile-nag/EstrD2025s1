{-
  * Stack (pila)
    Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
    que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
    platos). Su interfaz es la siguiente:
-}

module Stack (Stack, emptyS, isEmptyS, push, top, pop, lenS) where

data Stack a = S [a] Int

{-
    INV. REP.:
    - n es la cantidad de elementos de la pila
-}

-- Crea una pila vacía.
-- O(1)
emptyS :: Stack a
emptyS = S [] 0

-- Dada una pila indica si está vacía.
-- O(1)
isEmptyS :: Stack a -> Bool
isEmptyS (S lista _) = null lista

-- Dados un elemento y una pila, agrega el elemento a la pila.
-- O(1)
push :: a -> Stack a -> Stack a
push x (S lista n) = S (x : lista) (n + 1)

-- Dada una pila devuelve el elemento del tope de la pila.
-- O(1)
top :: Stack a -> a
top (S lista _) = head lista

-- Dada una pila devuelve la pila sin el primer elemento.
-- O(1)
pop :: Stack a -> Stack a
pop (S lista n) = S (tail lista) (n - 1)

-- Devuelve la cantidad de elementos en la pila.
-- O(1)
lenS :: Stack a -> Int
lenS (S _ n) = n
