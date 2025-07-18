#include <iostream>
using namespace std;

//2.
/*
 - Propósito: dados dos caracteres 'c1' y 'c2', imprime sus valores enteros (ASCII) desde 'c1' hasta 'c2' (inclusive), separados por ", ".
 - Precondición: c1 < c2
 - Resultado: >> printFromTo('a', 'e'); --> ???????????????
 - Consumo de memoria: 
    * Solo usa una variable local (i).
    * No hay recursión ni uso dinámico de memoria.
    * Uso de memoria -> O(1)
*/
void printFromTo(char c1, char c2){
    for (int i = 0; c1 + i <= c2; i++){
        cout << c1 + i << ", ";
    }
    cout << endl;
}

/*
 - Propósito: Devuelve el factorial del n pasado como argumento. 
 - Precondición: n >= 0
 - Resultado: >> fc(5); --> imprime: 120 
 - Consumo de memoria: 
    * Usa solo dos variables locales: x y n
    * No hay recursión.
    * Uso de memoria -> O(1)
*/
int fc(int n){
    int x = 1;
    while (n > 0){
        x = x * n;
        n--;
    }
    return x;
}

/*
 - Propósito: Dados dos números, realiza la suma del intervalo entre estos dos, incluyendolos. 
 - Precondición: n <= m
 - Resultado: >> fc(3, 7); --> imprime: 25 
 - Consumo de memoria: 
    * Usa llamadas recursivas
    * Cada llamada guarda un nuevo n en el stack frame
    * Uso de memoria -> O(m - n) siendo esta diferencia la cantidad de veces que se realiza la recursión
 - Podria mejorarse usando una iteración en lugar de una recursión
*/
int ft(int n, int m){
    if (n == m){
        return n;
    }
    return n + ft(n + 1, m);
}

//4

// Propósito: imprime n veces un string s. 
void printN(int n, string s) {
    for (int i = 0; i < n; i++){
        cout << s << " ";
    }
    cout << "Fin I"<< endl;
}

void printNR(int n, string s) {
    if (n > 0){
        cout << s << " ";
        printNR(n - 1, s);
    } else {
        cout << "Fin R" << endl;
    }
}

// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n) {
    while (n >= 0) {
        cout << n << endl;
        n--;
    }
    cout << "Fin I"<< endl;
}

void cuentaRegresivaR(int n) {
    if (n >= 0) {
        cout << n << endl;
        cuentaRegresivaR (n-1);
    } else {
        cout << "Fin R"<< endl;
    }
}

// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaN(int n){
    for (int i = 0; i <= n ; i++){
        cout << i << endl;
    }
}

void desdeCeroHastaNR(int n) {
    if(n>0){
        desdeCeroHastaNR(n-1);
    }

    cout<< n << endl;
}

// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int mult(int n, int m) {
    int resultado = 0;
    
    for (int i = 0; i < n; i++){
        resultado += m;
    }
    return resultado;
}

int multR(int n, int m) {
    return n == 0 || m==0 ? 0 : m + multR(n - 1, m);
}

// Propósito: imprime los primeros n char del string s, separados por un salto de línea. 
// Precondición: el string tiene al menos n char.
void primerosN(int n, string s){
    for (int i = 0; i < n; i++){
        cout << s[i] << endl;
    }
    cout << "Fin I" << endl;
}

void primerosNR(int n, string s) {
    if (n > 0) {
        primerosNR(n - 1, s);
        cout << s[n] << endl;
    }
}

// Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s) {
    bool esta = false;
    for (int i = 0; s[i] != '\0'; i++) {
        esta = esta || (s[i] == c);
    }
    return esta;
}

bool perteneceAux(char c, string s, int i) {
    return (s[i] != '\0') && ((s[i] == c) || perteneceAux(c, s, i + 1));
}

bool perteneceR(char c, string s) {
    return perteneceAux(c, s, 0);
}

// Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int apariciones(char c, string s){
    int result = 0;
    for (int i = 0; s[i] != '\0'; i++){
        if (s[i] == c) {
            result++;
        }
    }
    return result;
}

int aparicionesAux(char c, string s, int i) {
    int suma = (s[i] == c) ? 1 : 0;
    return suma + aparicionesAux(c, s, i + 1);
}

int aparicionesR(char c, string s) {
    return aparicionesAux(c, s, 0);
}

