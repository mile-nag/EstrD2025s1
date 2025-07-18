#include "Queue.h"
#include <iostream>
using namespace std;

// Crea una cola vacía. Costo: O(1).
Queue emptyQ(){
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
    return q;
}

//Indica si la cola está vacía. Costo: O(1).
bool isEmptyQ(Queue q){
    return q->cantidad == 0;
}

// Devuelve el primer elemento. Costo: O(1). //COMO MANDAR ERRORES SE PUEDE? ES RECOMENDABLE?
int firstQ(Queue q){
    if (q->cantidad > 0){ exit(1); } 
    return q->primero->elem;
}

// Agrega un elemento al final de la cola. Costo: O(1).
void Enqueue(int x, Queue q){
    NodoQ* nuevoQ = new NodoQ;
    nuevoQ->elem = x;
    nuevoQ->siguiente = NULL;

    if (q->cantidad == 0){
        q->primero = nuevoQ;
        q->ultimo = nuevoQ;
    } else {
        q->ultimo->siguiente = nuevoQ;
        q->ultimo = nuevoQ;
    }
    q->cantidad++;
}

// Quita el primer elemento de la cola. Costo: O(1).
void Dequeue(Queue q){
    NodoQ* actual = q->primero;
    q->primero = actual->siguiente;

    if (q->primero == NULL) {
        q->ultimo = NULL;
    }

    q->cantidad--; 
    delete actual;
}

// Devuelve la cantidad de elementos de la cola. Costo: O(1).
int lengthQ(Queue q){
    return q->cantidad;
}

// Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso. 
// Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos. 
// Costo: O(1).
void MergeQ(Queue q1, Queue q2){
    if (q1->cantidad > 0 && q2->cantidad > 0){
        q1->ultimo->siguiente = q2->primero;
        q1->ultimo = q2->ultimo;
        q1->cantidad += q2->cantidad;
    } else if (q1->cantidad == 0){
        q1->primero = q2->primero;
        q1->ultimo = q2->ultimo;
        q1->cantidad = q2->cantidad;
    }
    delete q2; //el enunciado dice que libera memoria de q2, pero NO de los nodos
} 
/*
MergeQ:

q1:     x  ->  y  ->  z     (primero=x, ultimo=z, cantidad=3)
q2:     w -> x              (primero=W, ultimo=x, cantidad=2)

 q1->ultimo->siguiente = q2->primero;
        x -> y -> z ------> w -> x   (porque w ya apunta a x)

 q1->cantidad += q2->cantidad;
        3 + 2 = 5
 
 delete q2; (solo elimina el header de q2, los nodos son apuntados por otra cosa)
*/

// Libera la memoria ocupada por la cola. Costo: O(n).
void DestroyQ(Queue q){
    NodoQ* actual = q->primero;
    while(actual != NULL){
        NodoQ* sig = actual->siguiente;
        delete actual;
        actual = sig;
    }
    delete q;
}
