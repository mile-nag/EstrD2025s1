#include <iostream>
#include "Set.h"
#include "Queue.h"

using namespace std;

// Función auxiliar para imprimir el Set usando
void printSet(Set s) {
    NodoS* nodo = s->primero;

    cout << "[ ";
    while (nodo != NULL) {
        cout << nodo->elem << " ";
        nodo = nodo->siguiente;
    }
    cout << "]" << endl;
}

void imprimirQueue(Queue q) {
    NodoQ* actual = q->primero;
    cout << "[ ";
    while (actual != NULL) {
        cout << actual->elem << " ";
        actual = actual->siguiente;
    }
    cout << "]" << endl;
}

int main() {
    /*   Set   */
    cout << "----------------------------------" << endl;
    cout << "------------- Set ----------------" << endl;
    cout << "----------------------------------" << endl;
    Set miSet = emptyS();

    cout << "agrego 1, 2 y 3 al set." << endl;
    AddS(1, miSet);
    AddS(2, miSet);
    AddS(3, miSet);
    printSet(miSet);  // Esperado (en orden inverso al agregado): [ 3 2 1 ]

    cout << "pertenece 2? " << (belongsS(2, miSet) ? "si" : "no") << endl;
    cout << "pertenece 4? " << (belongsS(4, miSet) ? "si" : "no") << endl;

    cout << "elimino el 2:" << endl;
    RemoveS(2, miSet);
    printSet(miSet);  // Esperado: [ 3 1 ]

    cout << "elimino el 3:" << endl;
    RemoveS(3, miSet);
    printSet(miSet);  // Esperado: [ 1 ]

    cout << "elimino el 1:" << endl;
    RemoveS(1, miSet);
    printSet(miSet);  // Esperado: [ ]

    cout << "esta vacio? " << (isEmptyS(miSet) ? "si" : "no") << endl;

    DestroyS(miSet);
    cout << "set destruido bip bop " << endl;

    /*   Queue   */

    cout << "----------------------------------" << endl;
    cout << "------------ Queue ---------------" << endl;
    cout << "----------------------------------" << endl;
    
    cout << "creando queue vacia q1" << endl;
    Queue q1 = emptyQ(); 

    cout << "enqueue 10, 20, 30 en q1" << endl;
    Enqueue(10, q1);
    Enqueue(20, q1);
    Enqueue(30, q1);

    cout << "como queda q1 despues de equeue: ";
    imprimirQueue(q1);

    cout << "primer elemento: " << firstQ(q1) << endl;
    cout << "cantidad total elementos: " << lengthQ(q1) << endl;

    Dequeue(q1);
    cout << "contenido de q1 despues de dequeue: ";
    imprimirQueue(q1);

    cout << "creando queue vacia q2" << endl;
    Queue q2 = emptyQ();
    Enqueue(100, q2);
    Enqueue(200, q2);

    cout << "contenido de q2: ";
    imprimirQueue(q2);

    MergeQ(q1, q2);

    cout << "contenido de q1 despues de merge q1 con q2: ";
    imprimirQueue(q1);

    cout << "cantidad total de elementos en q1: " << lengthQ(q1) << endl;

    cout << "queue destruida bip bop" << endl;
    DestroyQ(q1);

    return 0;
}