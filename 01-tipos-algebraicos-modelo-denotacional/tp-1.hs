-- 2. Números enteros

-- 2.1 Defina las siguientes funciones

-- 2.1.a Dado un número devuelve su sucesor
sucesor :: Int -> Int
sucesor x = x + 1

-- 2.1.b Dados dos números devuelve su suma utilizando la operación +.
sumar :: Int -> Int -> Int
sumar x y = x + y

-- 2.1.c Dado dos números, devuelve un par donde la primera componente es la división del primero por el segundo, y la segunda componente es el resto de dicha división.

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (division x y, mod x y)

-- AUX: Divide dos números, es parcial.
division :: Int -> Int -> Int
-- Precond.: x es distinto de 0
division x y = div x y

-- 2.1.d Dado un par de números devuelve el mayor de estos.
maxDelPar :: (Int, Int) -> Int
maxDelPar (x, y) =
  if x > y
    then x
    else y

-- 3. Tipos enumerativos

-- 3.1 Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar las siguientes funciones:

data Dir = Norte | Sur | Este | Oeste
  deriving (Show)

-- 3.1.a Dada una dirección devuelve su opuesta.
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

-- 3.1.b Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = True

-- 3.1.c Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existela siguiente dirección a Oeste.

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "No existe una dirección al oeste"

{- 3.2 Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves, Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones: -}

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
  deriving (Show)

-- 3.2.a Devuelve un par donde la primera componente es el primer día de la semana, y la segunda componente es el último día de la semana.

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDiaDeLaSemana, segundoDiaDeLaSemana)

primerDiaDeLaSemana :: DiaDeSemana
primerDiaDeLaSemana = Lunes

segundoDiaDeLaSemana :: DiaDeSemana
segundoDiaDeLaSemana = Domingo

-- 3.2.b Dado un día de la semana indica si comienza con la letra M.
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

{- 3.2.c Dado dos días de semana, indica si el primero viene después que el segundo. Analizar la calidad de la solución respecto de la cantidad de casos analizados
(entre los casosanalizados en esta y cualquier subtarea, deberían ser no más de 9 casos). Ejemplo: vieneDespues Jueves Lunes = True-}
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = diaANumero d1 > diaANumero d2

diaANumero :: DiaDeSemana -> Int
diaANumero Lunes = 1
diaANumero Martes = 2
diaANumero Miercoles = 3
diaANumero Jueves = 4
diaANumero Viernes = 5
diaANumero Sabado = 6
diaANumero Domingo = 7

-- 3.2.e Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

{- 3.3 Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina las siguientes funciones utilizando pattern matching
(no usar las funciones sobre booleanos ya definidas en Haskell):-}

-- 3.3.a Dado un booleano, si es True devuelve False, y si es False devuelve True. En Haskell ya está definida como not.
negar :: Bool -> Bool
negar True = False
negar False = True

-- 3.3.b Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino devuelve True. Esta función NO debe realizar doble pattern matching. Nota: no viene implementada en Haskell.
implica :: Bool -> Bool -> Bool
implica True b = b
implica False _ = True

-- 3.3.c Dados dos booleanos si ambos son True devuelve True, sino devuelve False. Esta función NO debe realizar doble pattern matching.
yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien False _ = False

-- 3.3.d Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False. Esta función NO debe realizar doble pattern matching. En Haskell ya está definida como ||.
oBien :: Bool -> Bool -> Bool
oBien False b = b
oBien True _ = True

-- 4. Registros

-- 4.1 Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:

data Persona = P String Int

-- 4.1.a Devuelve el nombre de una persona
nombre :: Persona -> String
nombre (P n _) = n

-- 4.1.b Devuelve la edad de una persona
edad :: Persona -> Int
edad (P _ e) = e

-- 4.1.c Aumenta en uno la edad de la persona.
crecer :: Persona -> Persona
crecer (P n e) = P n (e + 1)

-- 4.1.d Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nn (P n e) = P nn e

-- 4.1.e Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

-- 4.1.f Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 =
  if esMayorQueLaOtra p1 p2
    then p1
    else p2

{- 4.2 Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un porcentaje de energía; y Entrenador, como un nombre y dos Pokémon.
Luego definir las siguientes funciones: -}

data TipoDePokemon = Agua | Fuego | Planta
  deriving (Show)

data Pokemon = Pk TipoDePokemon Int
  deriving (Show)

data Entrenador = E String Pokemon Pokemon

-- 4.2.a Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = superaA' (tipo p1) (tipo p2)

superaA' :: TipoDePokemon -> TipoDePokemon -> Bool
superaA' Agua Fuego = True
superaA' Fuego Planta = True
superaA' Planta Agua = True
superaA' _ _ = False

tipo :: Pokemon -> TipoDePokemon
tipo (Pk t _) = t

-- 4.2.b Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ p1 p2) = boolANro (esDeMismoTipo t (tipo p1)) + boolANro (esDeMismoTipo t (tipo p2))

boolANro :: Bool -> Int
boolANro True = 1
boolANro _ = 0

esDeMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDeMismoTipo Agua Agua = True
esDeMismoTipo Fuego Fuego = True
esDeMismoTipo Planta Planta = True
esDeMismoTipo _ _ = False

-- 4.2.c Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (entrenador1, entrenador2) = agregar (pokemonesDe entrenador1) (pokemonesDe entrenador2)

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x : xs) ys = x : agregar xs ys

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (E _ poke1 poke2) = [poke1, poke2]

-- 5. Funciones polimórficas

-- 5.1. Defina las siguientes funciones polimórficas:

-- 5.1.a Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo :: a -> a
loMismo x = x

-- 5.1.b Dado un elemento de algún tipo devuelve el número 7.
siempreSiete :: a -> Int
siempreSiete x = 7

-- 5.1.c Dadas una tupla, invierte sus componentes. ¿Por qué existen dos variables de tipo diferentes?
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- 5.2. Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas? Porque una sola definicion opera sobre muchos tipos

-- 6. Pattern matching sobre listas

-- 6.1 Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no utilizar las funciones que ya vienen con Haskell):

-- 6.1.a Dada una lista de elementos, si es vacía devuelve True, sino devuelve False. Definida en Haskell como null.
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

-- 6.1.b Dada una lista devuelve su primer elemento. Definida en Haskell como head. Nota: tener en cuenta que el constructor de listas es :
elPrimero :: [a] -> a
elPrimero (x : xs) = x

-- 6.1.c Dada una lista devuelve esa lista menos el primer elemento.Definida en Haskell como tail. Nota: tener en cuenta que el constructor de listas es :
sinElPrimero :: [a] -> [a]
sinElPrimero (x : xs) = xs

-- 6.1.d Dada una lista devuelve un par, donde la primera componente es el primer elemento de la lista, y la segunda componente es esa lista pero sin el primero. Nota: tener en cuenta que el constructor de listas es :
splitHead :: [a] -> (a, [a])
splitHead (x : xs) = (x, xs)