#include "Entrenador.h"
#include <iostream>
using namespace std;

int main() {
    // ====== POKEMON ======
    Pokemon p1 = consPokemon("Fuego");
    Pokemon p2 = consPokemon("Planta");
    Pokemon p3 = consPokemon("Agua");
    Pokemon p4 = consPokemon("Fuego");

    cout << "Tipo de p4: " << tipoDePokemon(p4) << endl;
    cout << "Tipo de p1: " << tipoDePokemon(p1) << endl;

    cout << "Energia de p4: " << energia(p4) << "%" << endl;
    cout << "Energia de p1: " << energia(p1) << "%" << endl;

    perderEnergia(30, p4);
    perderEnergia(50, p1);

    cout << "Energia de p4 despues de perder 30: " << energia(p4) << "%" << endl;
    cout << "Energia de p1 despues de perder 50: " << energia(p1) << "%" << endl;

    if (superaA(p4, p1)) {
        cout << "p4 supera a p1" << endl;
    } else {
        cout << "p4 NO supera a p1" << endl;
    }

    // ====== ENTRENADOR ======
    // Equipos
    Pokemon* equipoAsh = new Pokemon[2];
    equipoAsh[0] = p4;
    equipoAsh[1] = p1;

    Pokemon* equipoMisty = new Pokemon[2];
    equipoMisty[0] = p3;
    equipoMisty[1] = p2;

    // Entrenadores
    Entrenador ash = consEntrenador("Ash", 2, equipoAsh);
    Entrenador misty = consEntrenador("Misty", 2, equipoMisty);

    // Funciones
    cout << "Entrenador: " << nombreDeEntrenador(ash) << " tiene " << cantidadDePokemon(ash) << " pokemon." << endl;
    cout << "Tipo del primer pokemon de Ash: " << tipoDePokemon(pokemonNro(0, ash)) << endl;

    // Buscamos por tipo Fuego
    cout << "Ash tiene " << cantidadDePokemonDe("Fuego", ash) << " de tipo Fuego." << endl;

    if (leGanaATodos(ash, misty)) {
        cout << "Ash le gana a todos los pokemon de Misty." << endl;
    } else {
        cout << "Ash NO le gana a todos los pokemon de Misty." << endl;
    }

    // ====== LIBERAR MEMORIA ======
    for (int i = 0; i < 2; i++) {
        delete equipoAsh[i];
        delete equipoMisty[i];
    }

    delete[] equipoAsh;
    delete[] equipoMisty;

    delete ash;
    delete misty;

    return 0;
}





