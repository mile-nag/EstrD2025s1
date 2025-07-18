
#include "Tree.h"
#include <iostream>
using namespace std;

// Estructura del nodo de la cola (ahora de tipo Tree)
struct NodoQ {
    Tree elem;         // actualizado a Tree
    NodoQ* siguiente;
};

// Estructura de la cola
struct QueueSt {
    int cantidad;
    NodoQ* primero;
    NodoQ* ultimo;
};

typedef QueueSt* QueueT;

// Crea una cola vacía. Costo: O(1).
QueueT emptyQ();

// Indica si la cola está vacía. Costo: O(1).
bool isEmptyQ(QueueT q);

// Devuelve el primer elemento. Costo: O(1).
Tree firstQ(QueueT q);

// Agrega un elemento al final de la cola. Costo: O(1).
void Enqueue(Tree x, QueueT q);

// Quita el primer elemento de la cola. Costo: O(1).
void Dequeue(QueueT q);

// Devuelve la cantidad de elementos de la cola. Costo: O(1).
int lengthQ(QueueT q);

// Anexa q2 al final de q1
void MergeQ(QueueT q1, QueueT q2);

// Libera la memoria ocupada por la cola. Costo: O(n).
void DestroyQ(QueueT q);
