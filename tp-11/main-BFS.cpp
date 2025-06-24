#include <iostream>
#include "TreeBFS.h"
#include "ArrayList.h"
using namespace std;
//g++ -o prueba main-BFS.cpp TreeBFS.cpp Tree.cpp QueueT.cpp ArrayList.cpp

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

    cout << "Suma de elementos: " << sumarT_BFS(t) << endl; // 75
    cout << "Cantidad de nodos: " << sizeT_BFS(t) << endl;  // 6

    int elem = 7;
    cout << elem << " esta en el arbol? " << (perteneceT(elem, t) ? "Si" : "No") << endl; // Si

    elem = 5;
    cout << "Apariciones de " << elem << ": " << aparicionesT(elem, t) << endl; // 1

    return 0;
}