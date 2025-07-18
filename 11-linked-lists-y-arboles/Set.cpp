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
/* Justificación de costos:
 * Costo operacional: O(1) –> solo se reserva memoria y se inicializan campos
 * Costo en memoria: O(1) -> solo se reserva espacio para el header
 */

// Indica si el conjunto está vacío.
bool isEmptyS(Set s){
    return s->cantidad == 0;
}
/* Justificación de costos:
 * Costo operacional: O(1) –> solo compara el valor de un campo
 * Costo en memoria: O(1) -> no se usan estructuras auxiliares
 */

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
/* Justificación de costos:
 * Costo operacional: O(N) –> peor caso el elemento esta al final y se recorre todo o no está y de igual manera recoore todo
 * Costo en memoria: O(1) -> solo se usan punteros auxiliares
 */

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
/* Justificación de costos:
 * Costo operacional: O(N) –> peor caso el elemento esta al final y se recorre todo o no está y de igual manera recoore todo
 * Costo en memoria: O(1) -> solo se usan punteros auxiliares
 */


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
/* Justificación de costos:
 * Costo operacional: O(N) –> hay que recorrer toda la lista (uso pertenece)
 * Costo en memoria: O(1) -> si el elemento ya existe no se crea nodo y si se agrega se reserva y no usa memoria significativa
 */

// Quita un elemento dado.
void RemoveS(int x, Set s){
    NodoS* curr = s->primero;
    // si el primero es el que hay que borrar
    if (curr != NULL && curr->elem == x) {
        s->primero = curr->siguiente;
        delete curr;
        s->cantidad--;
    } else { 
        // itera mientras haya siguiente, y el siguiente NO sea el que busco
        while (curr != NULL && curr->siguiente != NULL && curr->siguiente->elem != x) {
            curr = curr->siguiente;
        }
        // al salir del while, dos opciones: termine todo y no lo encontré o lo encontré
        if (curr != NULL && curr->siguiente != NULL) {
            NodoS* aBorrar = curr->siguiente;
            curr->siguiente = aBorrar->siguiente;
            delete aBorrar;
            s->cantidad--;
        } // si no entra al if, entonces el elemento no estaba en el conjunto
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
/* Justificación de costos:
 * Costo operacional: O(N) –> el elemento está al final o no esta y se recorre todo el set
 * Costo en memoria: O(1) -> solo se usa un puntero temporal y se libera un nodo
 */

// Devuelve la cantidad de elementos.
int sizeS(Set s){
    return s->cantidad;
}

/* Justificación de costos:
 * Costo operacional: O(1) –> acceso directo al campo cantidad
 * Costo en memoria: O(1) -> no se usan estructuras auxiliares
 */

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
/* Justificación de costos:
 * Costo operacional: O(N) –> se recorre todo el set y se agregan los elementos
 * Costo en memoria: O(N) -> se crean N nodos nuevos para la lista (costo de cada llamada a Cons)
 */

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
/* Justificación de costos:
 * Costo operacional: O(N) –> se recorre todo el set y se libera cada nodo
 * Costo en memoria: O(N) -> uso constante de punteros auxiliares 
 *                  (O SE USAN N PUNTEROS QUE DESP SE ELIMINAN NO SE COMO JUSTIFICAR)
 */

