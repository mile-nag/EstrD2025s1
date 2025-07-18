struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaño del array
};

typedef ArrayListSt* ArrayList;

// Crea una lista con 0 elementos. Nota: empezar el array list con capacidad 16.
ArrayList newArrayList();

// Crea una lista con 0 elementos y una capacidad dada por parámetro.
ArrayList newArrayListWith(int capacidad);

// Devuelve la cantidad de elementos existentes.
int lengthAL(ArrayList xs);

// Devuelve el iésimo elemento de la lista.
int get(int i, ArrayList xs);

// Reemplaza el iésimo elemento por otro dado.
void set(int i, int x, ArrayList xs);

// Decrementa o aumenta la capacidad del array. Nota: en caso de decrementarla, se pierden los elementos del nal de la lista.
void resize(int capacidad, ArrayList xs);

// Agrega un elemento al final de la lista.
void add(int x, ArrayList xs);

// Borra el último elemento de la lista.
void remove(ArrayList xs);