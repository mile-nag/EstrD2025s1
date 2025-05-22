#include <string>
using namespace std;

void printFromTo(char c1, char c2);
int fc(int n);
int ft(int n, int m);

// Propósito: imprime n veces un string s. 
void printN(int n, string s);
void printNR(int n, string s);

// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n);
void cuentaRegresivaR(int n);

// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaN(int n);
void desdeCeroHastaNR(int n, int m);

// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int mult(int n, int m);
int multR(int n, int m);

// Propósito: imprime los primeros n char del string s, separados por un salto de línea. 
// Precondición: el string tiene al menos n char.
void primerosN(int n, string s);
void primerosNR(int n, string s);