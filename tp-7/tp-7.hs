-- Ejercicio 2
-- Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los invariantes de BST y sin elementos repetidos (despreocuparse por el hecho
-- de que el árbol puede desbalancearse al insertar o borrar elementos). En todos los costos, N es la cantidad de elementos del árbol.
-- Justificar por qué la implementación satisface los costos dados.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use guards" #-}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
  deriving (Show)

-- Propósito: dado un BST dice si el elemento pertenece o no al árbol.
-- Costo: O(log N)
belongsBST :: (Ord a) => a -> Tree a -> Bool
belongsBST _ EmptyT = False
belongsBST x (NodeT y ti td) =
  (x == y) || belongsBST x (seguirPor x y ti td)

seguirPor :: (Ord a) => a -> a -> Tree a -> Tree a -> Tree a
seguirPor x y ti td =
  if x < y
    then ti
    else td

{- Justificación de costos:
   - N es el total de elementos en el arbol h es la altura de un arbol (la rama mas larga) => h = O(log N)
   - El costo de belongsBST es O(log N) en el peor caso se recorre una rama.
   - Esto se debe a que, en cada paso, la función compara el elemento buscado con el nodo actual y descarta una de las ramas (izquierda o derecha),
     reduciendo el problema a la mitad en cada llamado recursivo.
-}
-- Propósito: dado un BST inserta un elemento en el árbol.
-- Costo: O(log N)
insertBST :: (Ord a) => a -> Tree a -> Tree a
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) =
  if x < y
    then NodeT y (insertBST x ti) td
    else
      if x > y
        then NodeT y ti (insertBST x td)
        else NodeT y ti td

{- Justificación de costos:
   - N es el total de elementos en el árbol, h es la altura del árbol => el costo de recorrer una rama (en el peor caso, la rama entera) h = O(log N)
   - se realizan O(log N) llamadas recursivas.
     - Cada llamada realiza operaciones O(1) (comparacion y construccion de ndos)
     - costo total: O(log N).
-}

-- Propósito: dado un BST borra un elemento en el árbol.
-- Costo: O(log N)
-- deleteBST :: (Ord a) => a -> Tree a -> Tree a
-- deleteBST x EmptyT = EmptyT
-- deleteBST x (NodeT y ti td) =
-- NO SE QUE HACER

{-
-- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- Costo: O(log N)
splitMinBST :: (Ord a) => Tree a -> (a, Tree a)

-- Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- Costo: O(log N)
splitMaxBST :: (Ord a) => Tree a -> (a, Tree a)

-- Propósito: indica si el árbol cumple con los invariantes de BST.
-- Costo: O(N2)
esBST :: Tree a -> Bool

-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
-- Costo: O(log N)
elMaximoMenorA :: (Ord a) => a -> Tree a -> Maybe a

-- Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado.
-- Costo: O(log N)
elMinimoMayorA :: (Ord a) => a -> Tree a -> Maybe a

-- Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- Costo: O(N2)
balanceado :: Tree a -> Bool
-}

bst :: Tree Int
bst = NodeT 8 (NodeT 3 (NodeT 1 EmptyT EmptyT) (NodeT 6 (NodeT 4 EmptyT EmptyT) (NodeT 7 EmptyT EmptyT))) (NodeT 10 EmptyT (NodeT 14 (NodeT 13 EmptyT EmptyT) EmptyT))

{-
				       8
            /     \
			    3         10
			  /   \        \
			 1     6        14
			      / \      /
			     4   7    13

si quiero insertar 15 =
               8
            /     \
			    3         10
			  /   \        \
			 1     6        14
			      / \      /  \
			     4   7    13  15

>> insertBST 2 bst
   respuesta: NodeT 8 (NodeT 3 (NodeT 1 EmptyT (NodeT 2 EmptyT EmptyT)) (NodeT 6 (NodeT 4 EmptyT EmptyT) (NodeT 7 EmptyT EmptyT))) (NodeT 10 EmptyT (NodeT 14 (NodeT 13 EmptyT EmptyT) EmptyT))

-}
