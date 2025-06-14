#include "LinkedListV2.h"
#include <iostream>
using namespace std;

// Crea una lista vacía.
LinkedList nil(){
    LinkedListSt* xs = new LinkedListSt; //.Se genera el encabezado en la heap y obtengo el puntero.
    xs->primero = NULL;
    xs->ultimo = NULL;
    xs->cantidad = 0;

    return xs;
}

// Indica si la lista está vacía.
bool isEmpty(LinkedList xs){
    return xs->cantidad == 0;
}

// Devuelve el primer elemento.
int head(LinkedList xs){
    return xs->primero->elem;
}

// Agrega un elemento al principio de la lista.
void Cons(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem=x;
    nodo->siguiente = xs->primero;
    xs->primero = nodo;

    if(xs->ultimo == NULL){
        xs->ultimo = nodo;
    }

    xs->cantidad++;
}

// Quita el primer elemento.
void Tail(LinkedList xs){
    // PRECOND: lista no vacía
    NodoL* temp = xs->primero; // Guardo el puntero al primero de manera temporal
    xs->primero = xs->primero->siguiente; // Al primero, hacé que sea el primero que sigue

    if(xs->primero == NULL) { xs->ultimo = NULL; } // PARA CUMPLIR CON INVARIANTE!
    
    xs->cantidad-- ; // Modifico el tamaño
    delete temp; // Puedo borrar tranquilamente porque ya se que tengo
 
}

// Devuelve la cantidad de elementos.
int length(LinkedList xs);

// Agrega un elemento al final de la lista.
void Snoc(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem=x;
    nodo->siguiente= NULL;

    xs->ultimo= nodo;
    xs->cantidad++;
}

// Agrega todos los elementos de la segunda lista al final de los de la primera.
// La segunda lista se destruye
void Append(LinkedList xs, LinkedList ys){ // O(1)
    xs->ultimo->siguiente = ys->primero;
}

