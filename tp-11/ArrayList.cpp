#include "ArrayList.h"
#include <iostream>
using namespace std;

// Crea una lista con 0 elementos. 
// Nota: empezar el array list con capacidad 16.
ArrayList newArrayList(){
    ArrayList xs = new ArrayListSt;
    
    xs -> cantidad = 0;
    xs -> capacidad = 16;
    xs -> elementos = new int [16];

    return xs;
}

// Crea una lista con 0 elementos y una capacidad dada por parámetro.
ArrayList newArrayListWith(int capacidad){
    ArrayList newxs = new ArrayListSt;

    newxs -> cantidad = 0;
    newxs -> capacidad = capacidad;
    newxs -> elementos = new int [capacidad];
    
    return newxs;
}

// Devuelve la cantidad de elementos existentes.
int lengthAL(ArrayList xs){
    return xs->cantidad;
}

// Devuelve el iésimo elemento de la lista.
int get(int i, ArrayList xs){
    if (xs->cantidad < i) { exit (1); }
    return xs->elementos[i];
}

// Reemplaza el iésimo elemento por otro dado.
void set(int i, int x, ArrayList xs){
    if (i > xs->cantidad){ exit(1); }
    if (i >= 0) {
        xs->elementos[i] = x;
    }
}

// Decrementa o aumenta la capacidad del array. 
// Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
void resize(int capacidad, ArrayList xs){
    int* nuevoArray = new int[capacidad];

    int nuevaCantidad = (xs->cantidad < capacidad) ? xs->cantidad : capacidad;

    for (int i = 0; i < nuevaCantidad; i++) {
        nuevoArray[i] = xs->elementos[i];
    }

    delete[] xs->elementos;

    xs->elementos = nuevoArray;
    xs->capacidad = capacidad;
    xs->cantidad = nuevaCantidad;
}

// Agrega un elemento al final de la lista.
void add(int x, ArrayList xs){
    if (xs->cantidad + 1 > xs->capacidad) {
        resize(xs->capacidad * 2, xs);
    }  
    xs->elementos[xs->cantidad] = x;
    xs->cantidad++;
}

// Borra el último elemento de la lista.
void remove(ArrayList xs) {
    if (xs->cantidad > 0) {
        xs->cantidad--;
    }
}
