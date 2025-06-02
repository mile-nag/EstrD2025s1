#include "Ejercicio-4.h"
#include <iostream>
using namespace std;

// AUX. para mostrar la lista
void PrintList(ArrayList xs) {
    for (int i = 0; i < lengthAL(xs); i++) {
        cout << get(i, xs) << " ";
    }
    cout << endl;
}

int main() {
    ArrayList list = newArrayList();
    cout << "lista nueva: capacidad -> " << list->capacidad << " cantidad -> " << list->cantidad << endl;

    for (int i = 1; i <= 5; i++) {
        add(i * 10, list);
    }
    cout << "despues de agregar 5 elementos: ";
    PrintList(list);

    set(2, 99, list);
    cout << "despues de set(2, 99): ";
    PrintList(list);

    remove(list);
    cout << "despues de remove(): ";
    PrintList(list);

    resize(20, list);
    cout << "despues de resize(20): capacidad: " << list->capacidad << ", cantidad: " << list->cantidad << endl;

    add(100, list);
    add(200, list);
    cout << "despues de agregar 100 y 200: ";
    PrintList(list);

    resize(3, list);
    cout << "despues de resize(3): ";
    PrintList(list);

    ArrayList newList = newArrayListWith(5);
    cout << "lista personalizada  capacidad -> " << newList->capacidad << " cantidad -> " << newList->cantidad << endl;

    cout << "prueba de limites - get(10): ";
    int valor = get(10, list);
    cout << valor << endl;

    // PRUEBAS EJERCICIO 4 //

    ArrayList listA = newArrayList();
    for (int i = 1; i <= 5; i++) {
        add(i * 10, listA);
    }

    cout << "nueva lista: listA -> ";
    PrintList(listA);
    
    ArrayList listB = newArrayList();
    for (int i = 0; i <= 5; i++) {
        add(i*2, listB);
    }
    
    cout << "nueva lista: listB -> ";
    PrintList(listB);

    cout << "sumatoria de todos los elementos: "<< sumatoria(listA) << endl;

    sucesores(listA);
    cout << "despues de ejecutar 'sucesores': ";
    PrintList(listA);
    
    cout << "pertenece(3, listA) deberia ser falso, o sea, 0 -> " << pertenece(3, listA) <<endl;
    cout << "pertenece(41, listA) deberia ser true, o sea, 1 -> " << pertenece(41, listA) <<endl;

    cout << "apariciones(3, listA) deberia ser 0 -> " << apariciones(3, listA) << endl;
    cout << "apariciones(41, listA) deberia ser 1 -> " << apariciones(41, listA) << endl;

    cout << "listA -> ";
    PrintList(listA);

    cout << "listB -> ";
    PrintList(listB);

    ArrayList appendLS = append(listA, listB);
    cout << "append(listA, listB) -> ";
    PrintList(appendLS);

    return 0;
}