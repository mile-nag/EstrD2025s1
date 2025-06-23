#include "Tree.h"
#include "ArrayList.h"

// Dado un árbol binario de enteros devuelve la suma entre sus elementos.
int sumarT(Tree t){
    if(isEmptyT (t)) return 0;
    return rootT(t) + sumarT(right(t)) + sumarT(left(t));
}
/* Justificación de costos:
 * Costo operacional: O(N) ??????????
 * Costo en memoria: O(N), siendo N la cantidad de frames que se crean en memoria ya que la función es recursiva.
*/

// Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
int sizeT(Tree t){
    if (isEmptyT(t)) return 0;
    return 1 + sizeT(left(t)) + sizeT(right(t));
}

// Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
bool perteneceT(int e, Tree t){
    if (isEmptyT(t)) return false;
    return rootT(t) == e || perteneceT(e, left(t)) || perteneceT(e, right(t));
}

// AUX.: Devuelve 1 si la condición pasada como argumento es verdadera
int unoSi(bool condicion){
    return (condicion) ? 1 : 0;
}

// Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
int aparicionesT(int e, Tree t){
    if (isEmptyT(t)) return 0;
    return unoSi(rootT(t) == e) + aparicionesT(e, right(t)) + aparicionesT(e, left(t));
}

// AUX.: Dados dos números enteros devuelve el mayor entre estos
int max(int x, int y){
    return (x>=y) ? x : y;
}

// Dado un árbol devuelve su altura.
int heightT(Tree t){
    if(isEmptyT(t)) return 0;
    return 1 + max(heightT(left(t)), heightT(right(t)));
}


// AUX.: Agrega en orden los elementos del árbol dado a la lista 
void AgregarElementos(Tree t, ArrayList xs){
    if(!isEmptyT(t)){
        AgregarElementos(right(t), xs);
        add(rootT(t), xs); 
        AgregarElementos(left(t), xs);
    }
}

// Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t){
    ArrayList list = newArrayList();
    AgregarElementos(t, list);
    return list; 
}

// AUX.: Agrega los elementos del árbol dado en la lista dada.
void AgregarHojas(Tree t, ArrayList xs){
    if (!isEmptyT(t)) {
        if (isEmptyT(left(t)) && isEmptyT(right(t))) {
            add(rootT(t), xs);
        } else {
            AgregarHojas(left(t), xs);
            AgregarHojas(right(t), xs);
        }
    }
}

// Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t){
    ArrayList hojas = newArrayList();
    AgregarHojas(t, hojas);
    return hojas;
}

// AUX.: Agrega los valores del nivel pasado como argumento 
void agregarNivelN(int n, Tree t, ArrayList xs){
    if (!isEmptyT(t)) {
        if (n == 0) {
            add(rootT(t), xs);
        } else {
            agregarNivelN(n - 1, left(t), xs);
            agregarNivelN(n - 1, right(t), xs);
        }
    }
}

// Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
ArrayList levelN(int n, Tree t){
    ArrayList nivel = newArrayList(); 
    agregarNivelN(n, t, nivel);
    return nivel;
}










