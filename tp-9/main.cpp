#include <iostream>
#include "tp-9.h"
#include "Par.h"
#include "Fraccion.h"
using namespace std;

int main() {

    /* **** Par **** */
    Par p1 = consPar(3, 5);
    cout << "p1                     -> "; 
    ShowPar (p1);

    int first = fst (p1);
    cout << "fst (p1)               -> " << first << endl;

    int second = snd (p1);
    cout << "snd (p1)               -> " << second << endl;

    int maxDep1 = maxDelPar (p1);
    cout << "maxDelPar (p1)         -> " << second << endl;

    cout << "swap (p1)              -> ";
    ShowPar(swap(p1));

    cout << "divisionYResto (8, 8)  -> ";
    ShowPar(divisionYResto(8, 8));
    
    /* **** Funciones iterativas y recursivas **** */
    printN (3, "Hola");
    printNR (3, "Chau");

    cuentaRegresiva(3);
    cuentaRegresivaR(3);

    desdeCeroHastaN (4);
    desdeCeroHastaNR(4); 

    int test1 = mult(3,2);
    int test2 = multR(5,5);
    cout << "mult (3, 2)  -> " << test1 << endl;
    cout << "mult (5, 5)  -> " << test2 << endl;
    
    primerosN(3, "Milena");
    primerosNR(3, "Milena");
    //            M i l e n a 
    //            0|1|2|4|5|6

    cout << pertenece ('c', "casa") << endl;
    cout << perteneceR ('c', "auto") << endl;
    
    cout << apariciones('a', "aabbccaaddeeaa") << endl; // 6
    cout << aparicionesR('a', "aabbccaaddeeaa") << endl; // 6
    
    /* ****  Fracciones **** */
    Par p1 = consPar(3, 5);
    cout << "p1                     -> "; 
    ShowPar (p1);

    int first = fst (p1);
    cout << "fst (p1)               -> " << first << endl;

    int second = snd (p1);
    cout << "snd (p1)               -> " << second << endl;

    int maxDep1 = maxDelPar (p1);
    cout << "maxDelPar (p1)         -> " << second << endl;

    cout << "swap (p1)              -> ";
    ShowPar(swap(p1));

    cout << "divisionYResto (8, 8)  -> ";
    ShowPar(divisionYResto(8, 8));
    return 0;
}