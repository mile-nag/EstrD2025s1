#include "TreeBFS.h"
#include "QueueT.h"

// Dado un árbol binario de enteros devuelve la suma entre sus elementos usando un recorrido por niveles (BFS)
int sumarT_BFS(Tree t) {
    int suma = 0;
    QueueT q = emptyQ(); // new Queue
    
    if (!isEmptyT(t)) { // no 
        Enqueue(t, q);  // q = [10]
    }
    
    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q); // Tree = Nodo
        Dequeue(q);
        suma += rootT(actual);

        if (!isEmptyT(left(actual))) Enqueue(left(actual), q); 

        if (!isEmptyT(right(actual))) Enqueue(right(actual), q);

    }
    
    DestroyQ(q); // delete q
    return suma;
}

/*
       10                   10          [10 5 20 3 7 30]
      /  \  
     5    20                5 20
    / \     \
   3   7    30              3 7 30    suma => 10 + 5 + 20 + 3 + 7 + 30 = 75

    >> inicializamos la cola vacía y la suma en 0
        int suma = 0;
        QueueT q = emptyQ();
    
    >> el árbol ya está parado en su raiz, o sea 10.

    >> Primero: 
        ...
        if (!isEmptyT(t)) { // Si el árbol no es vacío, 
          Enqueue(t, q);    // se pone en la cola el árbol (o sea, 10)
        }
        ...
    
    ...Mientras la q NO sea vacía...

    >> Procesar el primer nodo:
        ...
        Tree actual = firstQ(q);  // obtiene la raiz de el árbol, que es 10 porque q = [10]
        Dequeue(q); // saco el 10 de la cola y queda: q = []
        suma += rootT(actual);    // 0 + 10 = 10
        
        // si no estan vacios, se meten el izquierdo y el derecho
        if (!isEmptyT(left(actual))) Enqueue(left(actual), q);   // 5
        if (!isEmptyT(right(actual))) Enqueue(right(actual), q); //20
        ... 

    >> Entonces, la suma queda 10 y la cola queda con [5, 20] (hijo izquierdo e hijo derecho )
        suma = 10
        q = [5,20] (que son los que faltan por procesar)

    >> Procesar el siguiente nodo: el 5 (el siguiente en la cola)
        ...
        Tree actual = firstQ(q); // obtiene el elem primero: 5 porque q = [5,20]  
        Dequeue(q);              // saca el 5 de la cola y queda  q = [20]
        suma += rootT(actual);   // 10 + 5 = 15
        
        // si no estan vacios, se meten el izquierdo y el derecho
        if (!isEmptyT(left(actual))) Enqueue(left(actual), q);  // 3
        if (!isEmptyT(right(actual))) Enqueue(right(actual), q);// 7
        ... 

    >> Entonces, la suma queda 15 y la cola queda con [20,3,7] (el que sigue, hijo izquierdo e hijo derecho de 5 )
        suma = 15
        q = [20,3,7] (que son los que faltan por procesar)

    >> Procesar el siguiente nodo: el 20 (el siguiente en la cola)
        ...
        Tree actual = firstQ(q); // obtiene el elem primero: 20 porque q = [20,3,7]  
        Dequeue(q);              // saca el 20 de la cola y queda  q = [3,7]
        suma += rootT(actual);   // 15 + 20 = 35
        
        // si no estan vacios, se meten el izquierdo y el derecho
        if (!isEmptyT(left(actual))) Enqueue(left(actual), q);  //  nada porque no existe
        if (!isEmptyT(right(actual))) Enqueue(right(actual), q);//  30
        ... 

    >> Entonces, la suma queda 35 y la cola queda con [3,7,30] 
        suma = 35
        q = [3,7,30]
    
    >> Procesar el siguiente nodo: el 3 (el siguiente en la cola)
        ...
        Tree actual = firstQ(q); // obtiene el elem primero: 3 porque q = [3,7,30]  
        Dequeue(q);              // saca el 3 de la cola y queda  q = [7,30]
        suma += rootT(actual);   // 35 + 3 = 38
        
        // 3 no tiene hijos, por lo tanto, no se encola nada...
        if (!isEmptyT(left(actual))) Enqueue(left(actual), q);   // NADA
        if (!isEmptyT(right(actual))) Enqueue(right(actual), q); // NADA
        ... 
    
    >> Entonces, la suma queda 38 y la cola queda con [7,30]
        suma = 38
        q = [7,30]

    >> Procesar el siguiente nodo: el 7 (el siguiente en la cola)
        ...
        Tree actual = firstQ(q); // obtiene el elem primero: 7 porque q = [7,30]  
        Dequeue(q);              // saca el 7 de la cola y queda q = [30]
        suma += rootT(actual);   // 38 + 7 = 45
        
        // 3 no tiene hijos, por lo tanto, no se encola nada...
        if (!isEmptyT(left(actual))) Enqueue(left(actual), q);   // NADA
        if (!isEmptyT(right(actual))) Enqueue(right(actual), q); // NADA
        ...

    >> Entonces, la suma queda 45 y la cola queda con [30]
        suma = 45
        q = [30]
    
    >> Procesar el siguiente nodo: el 30 (el siguiente y ultimo en la cola)
        ...
        Tree actual = firstQ(q); // obtiene el elem: 30 porque q = [30]  
        Dequeue(q);              // saca el 30 de la cola y queda q = []
        suma += rootT(actual);   // 45 + 30 = 75
        
        // 3 no tiene hijos, por lo tanto, no se encola nada...
        if (!isEmptyT(left(actual))) Enqueue(left(actual), q);   // NADA
        if (!isEmptyT(right(actual))) Enqueue(right(actual), q); // NADA
        ...
    
    >> Si la cola está vacía el bucle while termina, la suma queda en 75.

    Costos: 
    - O(N) donde N es el número de nodos
    - En memoria O(M): M donde M es el ancho máximo del árbol (máxima cantidad de nodos por nivel)
                       Es la cantidad maxima de nodos de la queue que ocupan simultaneamente un espacio en memoria
 
    Estructura del recorrido BFS:

    // 1. Crear estructura auxiliar (cola)
    QueueT q = emptyQ();

    // 2. Validar si el árbol está vacío
    if (!isEmptyT(t)) {
        Enqueue(t, q);  // 3. Encolar la raíz
    }

    // 4. Recorrer con un while
    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);

        // 5. Hacer lo que tengas que hacer con el nodo
        ... 

        // 6. Encolar hijos si existen
        if (!isEmptyT(left(actual))) Enqueue(left(actual), q);
        if (!isEmptyT(right(actual))) Enqueue(right(actual), q);
    }

    // 7. Liberar la estructura auxiliar
    DestroyQ(q);
 
 
    */

// Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
int sizeT_BFS(Tree t){
    int size = 0;
    QueueT q = emptyQ();

    if (!isEmptyT(t)) { Enqueue(t, q); }

    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);
        size++;

        // si no estan vacios, se meten el izquierdo y el derecho a la cola
        if (!isEmptyT(left(actual))) Enqueue(left(actual), q);
        if (!isEmptyT(right(actual))) Enqueue(right(actual), q);
    }
    
    DestroyQ(q);
    return size;
}

// Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
bool perteneceT(int e, Tree t){
    if (isEmptyT(t)) return false;

    QueueT q = emptyQ();
    Enqueue(t, q);

    bool encontrado = false;

    while (!isEmptyQ(q) && !encontrado) {
        Tree actual = firstQ(q);
        Dequeue(q);
        
        //ACA como evito hacer esto en el while? ?? ?? ? ? ?? 
        if (!isEmptyT(left(actual))) Enqueue(left(actual), q);
        if (!isEmptyT(right(actual))) Enqueue(right(actual), q);
        
        encontrado = rootT(actual) == e;
    }

    DestroyQ(q);
    return encontrado;
}

// Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
int aparicionesT(int e, Tree t){
    int visto = 0;
    QueueT q = emptyQ();

    if (!isEmptyT(t)) { Enqueue(t, q); }

    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);

        if (rootT(actual) == e) visto++;

        // si no estan vacios, se meten el izquierdo y el derecho a la cola
        if (!isEmptyT(left(actual))) Enqueue(left(actual), q);
        if (!isEmptyT(right(actual))) Enqueue(right(actual), q);
    }
    
    DestroyQ(q);
    return visto;
}