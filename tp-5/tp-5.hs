-- import Set (Set, addS, belongs, emptyS, setToList, unionS)
-- import Queue (Queue, dequeue, emptyQ, enqueue, firstQ, isEmptyQ)
-- import SetV2 (Set, addS, belongs, emptyS, setToList, unionS)
import QueueV2 (Queue, dequeue, emptyQ, enqueue, firstQ, isEmptyQ)
import Stack (Stack, emptyS, isEmptyS, lenS, pop, push, top)

-- 1. CÁLCULO DE COSTOS

-- Especificar el costo operacional de las siguientes funciones:

-- O(1) -- constante
head' :: [a] -> a
head' (x : xs) = x

-- O(1) -- constante
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- O(n) siendo n el int pasado por parametro -- lineal: se realiza una operacion constante n veces
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- O(n) siendo n la cantidad de elementos de la lista -- lineal
longitud :: [a] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

-- O(n * k) siendo n la cantidad de números de la lista y k el costo operacional de factorial (el valor máximo en la lista)
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x : xs) = factorial x : factoriales xs

-- O(m) siendo m la cantidad de elementos de la lista -- lineal: en el peor caso se recorre toda la lista buscando el elemento n pasado por parámetro
pertenece :: (Eq a) => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x : xs) = n == x || pertenece n xs

{-
-- O(n^2) siendo n la cantidad de elementos de la lista -- cuadratica: se realiza una operacion lineal (pertenece) por cada elemento de la lista
sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x : xs) =
  if pertenece x xs
    then sinRepetidos xs
    else x : sinRepetidos xs
-}
-- O(n) siendo n la cantidad de elementos de la primera lista recibida por parámetro -- lineal: se realiza una operacion constante por cada elemento
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

-- O(m) donde m es la suma de las longitudes de todas las strings -- lineal
concatenar :: [String] -> String
concatenar [] = []
concatenar (x : xs) = x ++ concatenar xs

-- O(n) siendo n la cantidad de elementos en la lista -- lineal
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x : xs) = x : takeN (n - 1) xs

-- O(n) siendo n la cantidad de elemntos de la lista
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x : xs) = dropN (n - 1) xs

-- O(n) siendo n el valor del primer argumento -- lineal
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

-- O(n) siendo n la cantidad de elementos de la lista -- lineal
minimo :: (Ord a) => [a] -> a
minimo [x] = x
minimo (x : xs) = min x (minimo xs)

-- O(n) siendo n la cantidad de elementos de la lista -- lineal: en el peor caso recorre toda la lista en busca del elemento a sacar
sacar :: (Eq a) => a -> [a] -> [a]
sacar n [] = []
sacar n (x : xs) =
  if n == x
    then xs
    else x : sacar n xs

-- O(n^2) siendo n la cantidad de elementos en la lista -- cuadrática: se realiza una operacion lineal por cada paso de la recursion
ordenar :: (Ord a) => [a] -> [a]
ordenar [] = []
ordenar xs =
  let m = minimo xs
   in m : ordenar (sacar m xs)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
   >> Set sin repetidos (conjunto)

   *  emptyS :: Set a
   *  addS :: Eq a => a -> Set a -> Set a
   *  belongs :: Eq a => a -> Set a -> Bool
   *  sizeS :: Eq a => Set a -> Int
   *  removeS :: Eq a => a -> Set a -> Set a
   *  unionS :: Eq a => Set a -> Set a -> Set a
   *  setToList :: Eq a => Set a -> [a]

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
  deriving (Show)

-- 2. Como usuario del tipo abstracto Set implementar las siguientes funciones:

-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen :: (Eq a) => [a] -> Set a -> [a]
losQuePertenecen [] _ = []
losQuePertenecen (x : xs) set =
  if x `belongs` set
    then x : losQuePertenecen xs set
    else losQuePertenecen xs set

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos xs = setToList (sinRepetidos' xs)

sinRepetidos' :: (Eq a) => [a] -> Set a
sinRepetidos' [] = emptyS
sinRepetidos' (x : xs) = addS x (sinRepetidos' xs)

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos :: (Eq a) => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT set ts1 ts2) = unionS set (unionS (unirTodos ts1) (unirTodos ts2))
-}

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
   >> Queue (cola)

   *  emptyQ :: Queue a
   *  isEmptyQ :: Queue a -> Bool
   *  enqueue :: a -> Queue a -> Queue a
   *  firstQ :: Queue a -> a
   *  dequeue :: Queue a -> Queue a
-}

-- 3. Como usuario del tipo abstracto Queue implementar las siguientes funciones:

-- Cuenta la cantidad de elementos de la cola.
lengthQ :: Queue a -> Int
lengthQ q =
  if isEmptyQ q
    then 0
    else 1 + lengthQ (dequeue q)

q1 = enqueue 1 (enqueue 2 (enqueue 3 emptyQ))

q2 = enqueue 11 (enqueue 22 (enqueue 33 emptyQ))

-- Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto.
queueToList :: Queue a -> [a]
queueToList q =
  if isEmptyQ q
    then []
    else firstQ q : queueToList (dequeue q)

-- Inserta todos los elementos de la segunda cola en la primera
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 =
  if isEmptyQ q2
    then q1
    else unionQ (enqueue (firstQ q2) q1) (dequeue q2)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
   >> Stack (pila)

   *  emptyS :: Stack a
   *  isEmptyS :: Stack a -> Bool
   *  push :: a -> Stack a -> Stack a
   *  top :: Stack a -> a
   *  pop :: Stack a -> Stack a
   *  lenS :: Stack a -> Int

-}

-- 1. Como usuario del tipo abstracto Stack implementar las siguientes funciones:

-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x : xs) = push x (apilar xs)

--  apilar [1,2,3,4,5,6] >> resultado >> S [1,2,3,4,5,6] 6

-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar s =
  if isEmptyS s
    then []
    else top s : desapilar (pop s)

-- desapilar st1 >> resultado >> [4,3,2,1,0]

st1 :: Stack Int
st1 = push 4 (push 3 (push 2 (push 1 (push 0 emptyS))))

-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
-- Precondición: la posición n debe ser válida
insertarEnPos 0 x stack = push x stack
insertarEnPos n x stack = push (top stack) (insertarEnPos (n - 1) x (pop stack))

-- insertarEnPos 0 5 st1 >> resultado >> S [5,4,3,2,1,0] 6
-- insertarEnPos 5 5 st1 >> resultado >> S [4,3,2,1,0,5] 6