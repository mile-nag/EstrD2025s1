#include "LinkedList.h"
#include <iostream>
using namespace std;

/*  LinkedList  */

// Crea una lista vacía.
LinkedList nil(){
   LinkedListSt* xs = new LinkedListSt;
   xs->cantidad = 0;
   xs->primero= NULL;

   return xs;
}
/* Justificación de costos nil():
 * Costo operacional: O(1) -> Crea una estructura vacía con valores iniciales
 * Costo en memoria: O(1) -> Reserva memoria para una LinkedListSt (un puntero)
*/

// Indica si la lista está vacía.
bool isEmpty(LinkedList xs){ // O(1)
    return xs->cantidad == 0;
}
/* Justificación de costos isEmpty(LinkedList xs):
 * Costo operacional: O(1) -> Solo accede al campo cantidad 
 * Costo en memoria: O(1) -> No usa memoria adicional significativa
*/

// Devuelve el primer elemento.
// Agrego la precondición de que la lista NO puede estar vacía
int head(LinkedList xs){ // O(1)
    return (xs->primero)->elem;  
}
/* Justificación de costos head(LinkedList xs):
 * Costo operacional: O(1) -> Accede al primer nodo directamente
 * Costo en memoria: O(1) -> No se crean estructuras nuevas // No se usa memoria adicional significativa
*/


// Agrega un elemento al principio de la lista.
void Cons(int x, LinkedList xs){ // O(1)
    NodoL* nodoNuevo = new NodoL; //creo el nodo nuevo para alm. el x

    nodoNuevo->elem = x; // asigno el elem x
    nodoNuevo->siguiente = xs->primero; // apunto al siguiente (anterior nuevo)

    xs->cantidad++; // aumento la cantidad 
    xs->primero = nodoNuevo; 
}
/* Justificación de costos Cons(int x, LinkedList xs):
 * Costo operacional: O(1) -> inserta al principio sin recorrer la lista
 * Costo en memoria: O(1) -> Crea solo un nodo (new NodoL) (un puntero)
*/

// Quita el primer elemento.
void Tail(LinkedList xs){ // O(1)
    NodoL* temp = (xs->primero)->siguiente;

    delete xs->primero; 
    
    xs->cantidad--;
    xs->primero = temp;
}
/* Justificación de costos Tail(LinkedList xs):
 * Costo operacional: O(1) -> Elimina el primer nodo sin recorrer
 * Costo en memoria: O(1) -> Libera un nodo
*/

// Devuelve la cantidad de elementos.
int length(LinkedList xs){ // O(1)
    return xs->cantidad;
}
/* Justificación de costos length(LinkedList xs):
 * Costo operacional: O(1) -> Solo accede al campo cantidad de la lista pasada como argumento
 * Costo en memoria: O(1) -> No usa memoria adicional
*/

// Agrega un elemento al final de la lista.
void Snoc(int x, LinkedList xs){ // O(N) - Lineal
    NodoL* nuevo= new NodoL;
    nuevo->elem = x;
    nuevo->siguiente = NULL;

    if (xs->primero == NULL) {
        // Si la lista está vacía, el nuevo nodo tiene que ser el primero
        xs->primero = nuevo;
    } else {
        // Si la lista NO está vacía: recorre hasta el final
        NodoL* curr = xs->primero; 
        while (curr->siguiente != NULL) {
            curr = curr->siguiente;
        }
        curr->siguiente = nuevo;
    }
    xs->cantidad++;
}
/* Justificación de costos Snoc(int x, LinkedList xs):
 * Costo operacional: O(N) -> realiza una operación O(1) N veces hasta llegar al último elemento de la lista 
 * Costo en memoria: O(1) -> Solo reserva un nodo nuevo (new NodoL)(puntero)
*/

/*   ListIterator   */
// Apunta el recorrido al primer elemento.
ListIterator getIterator(LinkedList xs){
    IteratorSt* ixs = new IteratorSt; // Tengo y puedo usar IteratorSt* porque soy implementador y es mi dato
    ixs->current = xs->primero;
    return ixs;
}
/* Justificación de costos getIterator(LinkedList xs):
 * Costo operacional: O(1) -> Solo apunta al primer nodo 
 * Costo en memoria: O(1) -> Reserva memoria para un iterador(un puntero)
*/

// Devuelve el elemento actual en el recorrido.
int current(ListIterator ixs){
    return ixs->current->elem;
}
/* Justificación de costos current(ListIterator ixs):
 * Costo operacional: O(1) -> Solo se accede a un campo y se devuelve
 * Costo en memoria: O(1) -> No usa memoria adicional
*/

// Reemplaza el elemento actual por otro elemento.
void SetCurrent(int x, ListIterator ixs){
    ixs->current->elem = x;
}
/* Justificación de costos SetCurrent(int x, ListIterator ixs):
 * Costo operacional: O(1) -> Solo se accede a un campo y asigna un valor
 * Costo en memoria: O(1) -> No usa memoria adicional
*/

// Pasa al siguiente elemento.
void Next(ListIterator ixs){
    ixs->current = ixs->current->siguiente;
}
/* Justificación de costos Next(ListIterator ixs):
 * Costo operacional: O(1) -> Solo cambia a donde apunta el puntero pasado como argumento
 * Costo en memoria: O(1) -> No usa memoria adicional
*/

// Indica si el recorrido ha terminado.
bool atEnd(ListIterator ixs){
    return ixs->current == NULL;
}
/* Justificación de costos atEnd(ListIterator ixs):
 * Costo operacional: O(1) -> Pregunta si el siguiente al que apunta es null
 * Costo en memoria: O(1) -> No usa memoria adicional
*/

// Libera la memoria ocupada por el iterador.
void DisposeIterator(ListIterator ixs){
    delete ixs;
}
/* Justificación de costos DisposeIterator(ListIterator ixs):
 * Costo operacional: O(1) -> libera el iterador
 * Costo en memoria: O(1) -> libera memoria del iterador
*/

// Libera la memoria ocupada por la lista.
void DestroyL(LinkedList xs){
    //Hay que recorrer la lista y liberar cada nodo antes de hacer el delete xs!!
    NodoL* curr = xs->primero;
    while (curr != NULL) {
        NodoL* temp = curr->siguiente;
        delete curr;
        curr = temp;
    }
    delete xs;
}
/* Justificación de costos DestroyL(LinkedList xs):
 * Costo operacional: O(N) -> Recorre la lista entera liberando cada nodo 
 * Costo en memoria: O(1) -> No se usa memoria adicional, solo se libera 
*/


