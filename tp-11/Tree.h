struct NodeT {
    int elem;
    NodeT* left;
    NodeT* right;
};

typedef NodeT* Tree;

// Construye un árbol vacío (árbol sin nodos)
Tree emptyT();
// Construye un árbol no vacío con un elemento y dos subárboles
Tree nodeT(int elem, Tree left, Tree right);
// Indica si el árbol está vacío (no tiene nodos)
bool isEmptyT(Tree t);
// Devuelve el valor de la raíz del árbol. PRECOND.: el árbol no debe estar vacío
int rootT(Tree t);
// Devuelve el subárbol izquierdo del árbol. PRECOND.: el árbol no debe estar vacío
Tree left(Tree t);
// Devuelve el subárbol derecho del árbol. PRECOND.: el árbol no debe estar vacío
Tree right(Tree t);