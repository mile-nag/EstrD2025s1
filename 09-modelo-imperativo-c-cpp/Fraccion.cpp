#include "Fraccion.h"
#include <iostream>
using namespace std;

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;

    return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f){
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f){
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return f.numerador / f.denominador;
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2){
    Fraccion result;
    result.numerador = f1.numerador * f2.numerador;
    result.denominador = f1.denominador * f2.denominador;
    return result;
}


// AUX. Proposito: indica si i es divisor de x
bool esDivisorDe(int i, int x){
    return x%i == 0;
}

// AUX. Proposito: devuelve el maximo comun divisor entre x e y
int maxComunDiv(int x, int y){
    int div = 1;
    for(int i = 1; i<=x && i<=y  ; i++){
        if (esDivisorDe(i, x) && esDivisorDe(i, y)){
            div = i;
        }
    }
    return div;
}


// Propósito: devuelve una fracción que resulta de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p){
    int mcDiv = maxComunDiv(p.numerador, p.denominador);

    p.numerador = p.numerador/mcDiv;
    p.denominador = p.denominador/mcDiv;

    return p;
}

// Propósito: devuelve la fracción resultante de sumar las fracciones
Fraccion sumF(Fraccion f1, Fraccion f2){
    Fraccion suma;

    suma.numerador = f1.numerador * f2.denominador + f2.numerador * f1.denominador;
    suma.denominador = f1.denominador * f2.denominador;

    return suma; // simplificada(suma);
}