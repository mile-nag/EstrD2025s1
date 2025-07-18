#include "Tree.h"
#include "User-Tree.cpp"
#include <iostream>
using namespace std;

// imprimir una lista
void printList(ArrayList list) {
    cout << "[";
    for (int i = 0; i < lengthAL(list); i++) {
        cout << get(i, list);
        if (i < lengthAL(list) - 1) cout << ", ";
    }
    cout << "]" << endl;
}

int main() {
    Tree t = nodeT(10, 
                  nodeT(5, 
                       nodeT(3, emptyT(), emptyT()), 
                       nodeT(7, emptyT(), emptyT())), 
                  nodeT(20, 
                       emptyT(), 
                       nodeT(30, emptyT(), emptyT())));

    /* Estructura del árbol:
            10
           /  \
          5    20
         / \     \
        3   7    30
    */

    // sumarT
    cout << "Suma de elementos: " << sumarT(t) << endl;  // 75

    // sizeT
    cout << "Cantidad de nodos: " << sizeT(t) << endl;   // 6

    // perteneceT
    int elem = 7;
    cout << elem << " esta en el arbol? " << (perteneceT(elem, t) ? "Si" : "No") << endl;  // si

    // aparicionesT
    elem = 5;
    cout << "Apariciones de " << elem << ": " << aparicionesT(elem, t) << endl;  // Debe dar 1

    // heightT
    cout << "Altura del arbol: " << heightT(t) << endl;  // 3

    // toList (in-order)
    cout << "Elementos: ";
    ArrayList list = toList(t);
    printList(list);  // [30, 20, 10, 7, 5, 3]

    // leaves (hojas)
    cout << "Hojas del arbol: ";
    ArrayList hojas = leaves(t);
    printList(hojas);  // [3, 7, 30]

    // levelN (nivel 1)
    cout << "Nodos en nivel: 1 ";
    ArrayList nivelN = levelN(1, t);
    printList(nivelN);  // [5, 20]

    // levelN (nivel 1)
    cout << "Nodos en nivel 2: " ;
    ArrayList nivelN2 = levelN(2, t);
    printList(nivelN2);  // [5, 20]

    return 0;
}