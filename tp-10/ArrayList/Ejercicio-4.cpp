#include "Ejercicio-4.h"
#include <iostream>
using namespace std;

/*
* Ejercicio 4
* Defnir las siguientes funciones utilizando la interfaz de ArrayList
*/

// Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs){
    int sum = 0;
    for (int i = 0; i<lengthAL (xs); i++){
       sum = get(i, xs) + sum;
    }
    return sum;
};

// Incrementa en uno todos los elementos.
void sucesores(ArrayList xs){
    for(int i = 0; i<lengthAL(xs); i++){
        set (i, get (i, xs) + 1, xs);
    }
}

// Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs){
    bool res = false;
    for(int i = 0; i < lengthAL(xs) || x == get(i, xs) ; i++){
        res = x == get(i, xs) || res;
    }
    return res;
}

// Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs){
    int cont = 0;
    for (int i = 0; i<lengthAL(xs); i++){
        if (x == get(i, xs)) { cont++; }
    }
    return cont;
}

// Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys) {
    ArrayList newA = newArrayListWith(lengthAL(xs) + lengthAL(ys));
    
    for(int i = 0; i < lengthAL(xs); i++) {
        add(get(i, xs), newA);
    }

    for(int j = 0; j < lengthAL(ys); j++) {
        add(get(j, ys), newA);
    }
    
    return newA;
}

// AUX. para mostrar la lista
void PrintList(ArrayList xs) {
    for (int i = 0; i < lengthAL(xs); i++) {
        cout << get(i, xs) << " ";
    }
    cout << endl;
}

int main(){
    ArrayList list = newArrayList();
    cout << "lista nueva: capacidad -> " << list->capacidad << " cantidad -> " << list->cantidad << endl;

    for (int i = 1; i <= 5; i++) {
        add(i * 10, list);
    }

    cout << "despues de agregar 5 elementos: ";
    PrintList(list);

    cout << "sumatoria de todos los elementos: "<< sumatoria(list) << endl;

    sucesores(list);
    cout << "despues de ejecutar 'sucesores': ";
    PrintList(list);
    
    cout << "pertenece(3, list) deberia ser falso, o sea, 0 -> " << pertenece(3, list) <<endl;
    cout << "pertenece(41, list) deberia ser true, o sea, 1 -> " << pertenece(41, list) <<endl;

    cout << "apariciones(3, list) deberia ser 0 -> " << apariciones(3, list) << endl;
    cout << "apariciones(41, list) deberia ser 1 -> " << apariciones(41, list) << endl;

    ArrayList listC = newArrayList();
    for (int i = 0; i <= 5; i++) {
        add(i*2, listC);
    }
    
    cout << "lista original: list -> ";
    PrintList(list);

    cout << "nueva lista: listC -> ";
    PrintList(listC);

    ArrayList appendLS = append(list, listC);
    cout << "append(list, listC) -> ";
    PrintList(appendLS);

}