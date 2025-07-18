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
/* Justificación de costos nil():
 * Costo operacional: O(1) -> Solo asigna los campos de la lista creada y la devuelve
 * Costo en memoria: O(1) -> Solo reserva un espacio nuevo para la nueva lista
*/


// Indica si la lista está vacía.
bool isEmpty(LinkedList xs){
    return xs->cantidad == 0;
}
/* Justificación de costos isEmpty(LinkedList xs):
 * Costo operacional: O(1) -> Solo accede a un campo y lo compara con un número
 * Costo en memoria: O(1) -> No hace uso de memoria significativa.
*/

// Devuelve el primer elemento.
int head(LinkedList xs){
    return xs->primero->elem;
}
/* Justificación de costos head(LinkedList xs):
 * Costo operacional: O(1) -> Solo accede a un campo y lo devuelve
 * Costo en memoria: O(1) -> No hace uso de memoria significativa.
*/

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
/* Justificación de costos Cons(int x, LinkedList xs):
 * Costo operacional: O(1) -> Solo asigna valores a los campos de el nuevo nodo y lo agrega
 * Costo en memoria: O(1) -> Sólo reserva el espacio para un nuevo nodo
*/


// Quita el primer elemento.
void Tail(LinkedList xs){
    // PRECOND: lista no vacía
    NodoL* temp = xs->primero; // Guardo el puntero al primero de manera temporal
    xs->primero = xs->primero->siguiente; // Al primero, hacé que sea el primero que sigue

    if(xs->primero == NULL) { xs->ultimo = NULL; } // PARA CUMPLIR CON INVARIANTE!
    
    xs->cantidad-- ; // Modifico el tamaño
    delete temp; // Puedo borrar tranquilamente porque ya se que tengo
}
/* Justificación de costos Tail(LinkedList xs):
 * Costo operacional: O(1) -> Solo reasigna valores a campos para reacomodar la lista 
 * Costo en memoria: O(1) -> GUarda un puntero de manera temporal y despues lo libera
*/


// Devuelve la cantidad de elementos.
int length(LinkedList xs){
    return xs->cantidad;
}
/* Justificación de costos length(LinkedList xs):
 * Costo operacional: O(1) -> Solo devuelve el valor de un campo
 * Costo en memoria: O(1) -> No usa memoria significativa.
*/

// Agrega un elemento al final de la lista.
void Snoc(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem=x;
    nodo->siguiente= NULL;

    if (xs->ultimo != NULL) {
        xs->ultimo->siguiente = nodo;
    } else {
        // Si la lista estaba vacía, también actualiza el primero
        xs->primero = nodo;
    }
    xs->ultimo = nodo;
    xs->cantidad++;
}
/* Justificación de costos Snoc(int x, LinkedList xs):
 * Costo operacional: O(1) -> Solo asigna valores a los campos del nuevo nodo y realiza algunos cambios minimos.
 * Costo en memoria: O(1) -> Solo reserva memoria para un nuevo nodo a agregar a la lista
*/

// Agrega todos los elementos de la segunda lista al final de los de la primera.
// La segunda lista se destruye
void Append(LinkedList xs, LinkedList ys){ // O(1)
    xs->ultimo->siguiente = ys->primero;
}
/* Justificación de costos Append(LinkedList xs, LinkedList ys):
 * Costo operacional: O(1) -> Solo reasigna el valor de un campo
 * Costo en memoria: O(1) -> No usa memoria significativa
*/

