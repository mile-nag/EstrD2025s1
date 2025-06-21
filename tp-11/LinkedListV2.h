#include <iostream>
using namespace std;

struct NodoL { //INV. REP: No SON COMPARTIDOS(?/(?/?()))
    int elem; // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {
  /* INV.REP.: 
   * first == NULL si y solo si last == NULL
   * cantidad es la cant de elementos en la lista hasta llegar a NULL desde first
   * si last != NULL entonces el siguiente es NULL y last es un puntero al último
  */
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
    NodoL* ultimo; // puntero al último nodo => agregado en esta versión
};

typedef LinkedListSt* LinkedList;

// Crea una lista vacía.
LinkedList nil();

// Indica si la lista está vacía.
bool isEmpty(LinkedList xs);

// Devuelve el primer elemento.
int head(LinkedList xs);

// Agrega un elemento al principio de la lista.
void Cons(int x, LinkedList xs);

// Quita el primer elemento.
void Tail(LinkedList xs);

// Devuelve la cantidad de elementos.
int length(LinkedList xs);

// Agrega un elemento al final de la lista.
void Snoc(int x, LinkedList xs); // O(N)

// Agrega todos los elementos de la segunda lista al final de los de la primera.
// La segunda lista se destruye
void Append(LinkedList xs, LinkedList ys);
