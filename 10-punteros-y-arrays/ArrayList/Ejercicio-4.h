#include "ArrayList.h"

// Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs);

// Incrementa en uno todos los elementos.
void sucesores(ArrayList xs);

// Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs);

// Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs);

// Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys);