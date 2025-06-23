#include "Tree.h"
#include "QueueT.h"
#include <iostream>
using namespace std;

// Crea una cola vacía. Costo: O(1).
QueueT emptyQ() {
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
    return q;
}

// Indica si la cola está vacía. Costo: O(1).
bool isEmptyQ(QueueT q) {
    return q->cantidad == 0;
}

// Devuelve el primer elemento. Costo: O(1).
Tree firstQ(QueueT q) {  
    if (isEmptyQ(q)) { exit(1);}
    return q->primero->elem;
}

// Agrega un elemento al final de la cola. Costo: O(1).
void Enqueue(Tree x, QueueT q) {  // Cambiado de int a Tree
    NodoQ* nuevoQ = new NodoQ;
    nuevoQ->elem = x;
    nuevoQ->siguiente = NULL;

    if (q->cantidad == 0) {
        q->primero = nuevoQ;
        q->ultimo = nuevoQ;
    } else {
        q->ultimo->siguiente = nuevoQ;
        q->ultimo = nuevoQ;
    }
    q->cantidad++;
}

// Quita el primer elemento de la cola. Costo: O(1).
void Dequeue(QueueT q) {
    if (isEmptyQ(q)) { exit(1); }
    
    NodoQ* actual = q->primero;
    q->primero = actual->siguiente;

    if (q->primero == NULL) {
        q->ultimo = NULL;
    }

    delete actual;
    q->cantidad--;
}

// Devuelve la cantidad de elementos de la cola. Costo: O(1).
int lengthQ(QueueT q) {
    return q->cantidad;
}

// Anexa q2 al final de q1
void MergeQ(QueueT q1, QueueT q2) {
    if (q1->cantidad > 0 && q2->cantidad > 0) {
        q1->ultimo->siguiente = q2->primero;
        q1->ultimo = q2->ultimo;
        q1->cantidad += q2->cantidad;
    } else if (q1->cantidad == 0) {
        q1->primero = q2->primero;
        q1->ultimo = q2->ultimo;
        q1->cantidad = q2->cantidad;
    }
    delete q2;
}

// Libera la memoria ocupada por la cola. Costo: O(n).
void DestroyQ(QueueT q) {
    NodoQ* actual = q->primero;
    while (actual != NULL) {
        NodoQ* sig = actual->siguiente;
        delete actual;
        actual = sig;
    }
    delete q;
}