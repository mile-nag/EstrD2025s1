#include "Entrenador.h"
#include <iostream>
using namespace std;

// Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve un entrenador.
Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    Entrenador e = new EntrenadorSt;
    e -> nombre = nombre;
    e -> pokemon = pokemon;
    e -> cantPokemon = cantidad;
    return e; 
}

// Devuelve el nombre del entrenador.
string nombreDeEntrenador(Entrenador e){
    return e -> nombre;
}

// Devuelve la cantidad de pokémon que posee el entrenador.
int cantidadDePokemon(Entrenador e){
    return e-> cantPokemon;
}

// Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int cant = 0;
    for(int i = 0; i<e->cantPokemon; i++){
       if (tipo == (tipoDePokemon(e->pokemon[i]))){
        cant++;
       }
    }
    return cant;
}

// Devuelve el pokémon número i de los pokémon del entrenador. Precondición: existen al menos i − 1 pokémon.
Pokemon pokemonNro(int i, Entrenador e){
    return e->pokemon[i];
}

// Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero posee al 
// menos un pokémon que le gane.  

bool pokeLeGanaATodos(Pokemon p, Entrenador e2){
    bool res = true;
    for(int i = 0; i < e2->cantPokemon ; i++){
        res = (superaA(p, e2->pokemon[i])) && res;
    }
    return res; 
}

bool leGanaATodos(Entrenador e1, Entrenador e2){
    bool res = false;
    for(int i = 0; i < e1->cantPokemon; i++){
        res = pokeLeGanaATodos(e1->pokemon[i], e2) || res;
    }
    return res; 
}


