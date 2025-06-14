#include "Set.h"
#include <iostream>
using namespace std;

// Crea un conjunto vacío.
Set emptyS(){
    SetSt* s = new SetSt;
    s->cantidad=0;
    s->primero=NULL;
    return s;
}

// Indica si el conjunto está vacío.
bool isEmptyS(Set s){
    return s->cantidad == 0;
}

// Indica si el elemento pertenece al conjunto.
bool belongsS(int x, Set s){
    NodoS* nodo = s->primero;

    bool belongs = false;
    while (nodo != NULL && !belongs){
        belongs = x == nodo->elem;
        nodo = nodo->siguiente;
    }
    return belongs;
}

// AUX.: denota verdadero si el elemento ya existe en el set
bool pertenece(int x, Set s){
    NodoS* nodo = s->primero;

    bool belongs = false;
    while (nodo != NULL && !belongs){
        belongs = x == nodo->elem;
        nodo = nodo->siguiente;
    }
    return belongs;
}


// Agrega un elemento al conjunto.
void AddS(int x, Set s){
    if (!pertenece(x, s)){
        NodoS* nuevoN = new NodoS;
        nuevoN->elem = x;
        nuevoN->siguiente = s->primero;

        s->primero = nuevoN;
        s->cantidad++;
    }
}

// Quita un elemento dado.
void RemoveS(int x, Set s){
    
    NodoS* curr = s->primero;

    // si el primero es el que hay que borrar
    if (curr != NULL && curr->elem == x) {
        s->primero = curr->siguiente; // el primero pasa a ser el siguiente
        delete curr;                  // borro el anterior
        s->cantidad--;
        return;                       // para salir de la función
    }

    // Si el primero NO era
    while (curr != NULL && curr->siguiente != NULL) {   // no es null y el siguiente NO es null
        if (curr->siguiente->elem == x) {               // si el siguiente es igual al que estoy buscando 
            NodoS* aBorrar = curr->siguiente;            // entonces el nodo a borrar es el siguiente
            curr->siguiente = aBorrar->siguiente;        // el siguiente al current, pasa a ser el siguiente del que borre antes
            delete aBorrar;                              // borro el que si o si tengo que borrar
            s->cantidad--;
            break;                                      // salgo del bucle
        }
        curr = curr->siguiente;                         // SI NO, paso al siguiente nodo y vuelvo a preguntar si es o no el que quiero
    }
}
/*
>> mi set
 header
   |
   v
+-------+     +-------+     +-------+
|   x   | --> |   y   | --> |   z   |
+-------+     +-------+     +-------+

>> quiero borrar el y:    estoy parado en x (el primero) -> guardo el segundo -> aBorrar = curr->siguiente;    

 header
   |                +---------------> aBorrar
   v                |            
+-------+     +-----|-+     +-------+
|   x   | --> |   y   | --> |   z   |
+-------+     +-------+     +-------+


>> curr->siguiente = aBorrar->siguiente;    --> x saltea a y y ahora apunta a z (que es el siguiente del que quiero borrar)

 header
   |                +---------------> aBorrar
   v                |            
+-------+     +-----|-+     +-------+
|   x   | -+  |   y   | --> |   z   |
+-------+  |  +-------+     +--|----+
           |                   |
           +-------------------+

>> delete aBorrar;                          --> liberar y

 header
   |                
   v                            
+-------+                   +-------+
|   x   | -+                |   z   |
+-------+  |                +--|----+
           |                   |
           +-------------------+
*/


// Devuelve la cantidad de elementos.
int sizeS(Set s){
    int cont = 0;
    while (s->primero != NULL){
        cont++;
    }
    return cont;
}

// Devuelve una lista con los lementos del conjunto.
LinkedList setToList(Set s){
    LinkedList list = nil();
    NodoS* curr = s->primero;

    while(curr != NULL){
        Cons(curr->elem, list);
        curr = curr->siguiente;
    }

    return list;
}

// Libera la memoria ocupada por el conjunto
void DestroyS(Set s){
   
    //Hay que recorrer el set y liberar cada nodo antes de hacer el delete total del set!
    NodoS* curr = s->primero;
    while (curr != NULL) {
        NodoS* temp = curr->siguiente;
        delete curr;
        curr = temp;
    }
    delete s;

}

