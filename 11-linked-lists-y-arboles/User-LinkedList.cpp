#include "LinkedList.h"
#include <iostream>
using namespace std;

// Devuelve la suma de todos los elementos.
int sumatoria(LinkedList xs){ // O(N)
    ListIterator i = getIterator (xs);

    int total = 0;

    while(!atEnd(i)){
        total += current(i);
        Next(i);
    }
    DisposeIterator(i);
    return total;
}
/* Justificación de costos sumatoria(LinkedList xs):
 * Costo operacional: O(N) -> Recorre toda la lista sumando cada elemento
 * Costo en memoria: O(1) -> Solo usa una variable acumuladora y un iterador
*/

// Incrementa en uno todos los elementos.
void Sucesores(LinkedList xs){ // O(N)
    ListIterator i = getIterator (xs);
    while(!atEnd(i)){
        SetCurrent( (current(i)+1) , i);
        Next(i);
    }
    DisposeIterator(i);
}
/* Justificación de costos Sucesores(LinkedList xs):
 * Costo operacional: O(N) -> Recorre toda la lista incrementando en 1 a cada elemento 
 * Costo en memoria: O(1) -> Solo usa un iterador
*/

// Indica si el elemento pertenece a la lista.
bool pertenece(int x, LinkedList xs){ // O(N)
    ListIterator i = getIterator(xs); 

    while(!atEnd(i) && !(x == current(i))){ //PRESTAR ATENCÓN -> Short circuit
        Next(i);
    }

    bool encontrado = x == current(i);

    DisposeIterator(i);

    return encontrado;
}
/* Justificación de costos pertenece(int x, LinkedList xs):
 * Costo operacional: O(N) -> En peor caso recorre toda la lista en busca del elemento pasado como argumento
 * Costo en memoria: O(1) -> Solo usa un iterador
*/

// Aux.: devuelve 1 si la condición es verdadera, 0 en caso contrario
int unoSi(bool condicion){ // O(1)
    return (condicion) ? 1 : 0;
}
/* Justificación de costos unoSi(bool condicion):
 * Costo operacional: O(1) -> No depende de ninguna estructura, solo evalúa una condición y devuelve un valor
 * Costo en memoria: O(1) -> No se usa memoria adicional
*/

// Indica la cantidad de elementos iguales a x.
int apariciones(int x, LinkedList xs){ // O(N)
    ListIterator i = getIterator(xs);
    int visto = 0;

    while(!atEnd(i)){
        visto += unoSi(x == current(i));
        Next(i);
    }
    DisposeIterator(i);
    return visto;
}
/* Justificación de costos apariciones(int x, LinkedList xs):
 * Costo operacional: O(N) -> Recorre toda la lista contando las apariciones del elemento pasado como arugmento
 * Costo en memoria: O(1) -> Solo usa un iterador y un acumulador
*/

//AUX.: Devuelve el mínimo entre dos números, si es igual, devuelve el primero
int min (int x, int y){ // O(1)
    return (x <= y) ? x : y;
}
/* Justificación de costos  min (int x, int y):
 * Costo operacional: O(1) -> No depende de ninguna estructura, solo evalúa una condición y devuelve un valor
 * Costo en memoria: O(1) -> No reserva nada nuevo
*/


// Devuelve el elemento más chico de la lista.
int minimo(LinkedList xs){ // O(N)
    ListIterator i = getIterator(xs);

    int minimoVisto = current(i);

    while (!atEnd(i)){
        minimoVisto = min(minimoVisto, current(i));
        Next(i);
    }
    DisposeIterator(i);
    return minimoVisto;
}
/* Justificación de costos  minimo(LinkedList xs):
 * Costo operacional: O(N) -> Recorre toda la lista buscando el mínimo de todos los elementos
 * Costo en memoria: O(1) -> Usa un iterador y una variable teporal para comparar valores
*/

// Dada una lista genera otra con los mismos elementos, en el mismo orden.
/* Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo? 
    Se podría mejorar sii se modifica la estructura de la linked list (o sea, agregar 
    más información al header: un campo 'last' o 'último' que apunte al último elemento)
*/
LinkedList copy(LinkedList xs){ // O(N^2) Se realiza N veces una operación O(N) 
    LinkedList nuevaL = nil();

    ListIterator i = getIterator(xs);

    while (!atEnd(i)){
        Snoc(current(i), nuevaL);
        Next(i);
    }
    DisposeIterator(i);
    return nuevaL;
}
/* Justificación de costos copy(LinkedList xs):
 * Costo operacional: O(N^2) -> Recorre toda la lista y por cada elemento se realiza Snoc con un costo operacional de O(N), esto se reliza N veces => O(N^2)
 * Costo en memoria: O(N) -> Crea una nueva lista con N nodos, uno por cada elemento.
*/

// Agrega todos los elementos de la segunda lista al final de los de la primera.
// La segunda lista se destruye.

void Append(LinkedList xs, LinkedList ys){ // O(N^2) Se realiza N veces una operación O(N) 
    ListIterator i = getIterator(ys);

    while (!atEnd(i)){
        Snoc(current(i), xs);
        Next(i);
    }
    DestroyL(ys);
    DisposeIterator(i);
}
/* Justificación de costos copy(LinkedList xs):
 * Costo operacional: O(N^2) -> Usa una operación Snoc en cada iteración y se realiza N veces O(N)
 * Costo en memoria: O(1) -> No crea nodos nuevos
*/

// Función auxiliar para mostrar la lista
void mostrarLista(LinkedList xs) {
    ListIterator it = getIterator(xs);
    cout << "[ ";
    while (!atEnd(it)) {
        cout << current(it) << " ";
        Next(it);
    }
    cout << "]" << endl;
    DisposeIterator(it);
}

int main() {
    // Crear lista vacía
    LinkedList lista = nil();
    
    // Agregar elementos: [1, 2, 3]
    Cons(3, lista);
    Cons(2, lista);
    Cons(1, lista);
    
    cout << "Lista original: ";
    mostrarLista(lista);

    // Snoc agrega al final
    Snoc(4, lista);
    Snoc(5, lista);
    cout << "Despues de Snoc(4) y Snoc(5): ";
    mostrarLista(lista);

    // Sumatoria
    cout << "Sumatoria: " << sumatoria(lista) << endl;

    // Sucesores
    Sucesores(lista);
    cout << "Despues de sucesores: ";
    mostrarLista(lista);

    // Pertenece
    cout << "Pertenece 3? " << (pertenece(3, lista) ? "Sí" : "No") << endl;
    cout << "Pertenece 99? " << (pertenece(99, lista) ? "Sí" : "No") << endl;

    // Apariciones
    Snoc(3, lista);
    cout << "Despues de agregar otro 3: ";
    mostrarLista(lista);
    cout << "Apariciones de 3: " << apariciones(3, lista) << endl;

    // Mínimo
    cout << "Minimo: " << minimo(lista) << endl;

    // Copiar lista
    LinkedList copia = copy(lista);
    cout << "Copia de la lista: ";
    mostrarLista(copia);

    // Append
    LinkedList otra = nil();
    Cons(100, otra);
    Cons(200, otra);
    cout << "Otra lista: ";
    mostrarLista(otra);

    Append(lista, otra); // ahora lista tiene todo
    cout << "Lista despues de append con otra: ";
    mostrarLista(lista);

    // Liberar memoria
    DestroyL(lista);
    DestroyL(copia);
    
    return 0;
}