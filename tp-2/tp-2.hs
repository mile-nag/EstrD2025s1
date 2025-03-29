{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}

-- 1. Recursión sobre listas

-- Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique lo contrario:

-- 1.1 Dada una lista de enteros devuelve la suma de todos sus elementos.

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n : ns) = n + sumatoria ns

-- 1.2 Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud :: [a] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

-- 1.3 Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n : ns) = (n + 1) : sucesores ns

-- 1.4 Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (b : bs) = b && conjuncion bs

-- 1.5 Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b : bs) = b || disyuncion bs

-- 1.6 Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs : xss) = xs ++ aplanar xss

-- 1.7 Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: (Eq a) => a -> [a] -> Bool
pertenece y [] = False
pertenece y (x : xs) = y == x || pertenece y xs

-- 1.8 Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.

apariciones :: (Eq a) => a -> [a] -> Int
apariciones y [] = 0
apariciones y (x : xs) = unoSi (y == x) + apariciones y xs

{- DEFINIDA EN LA MISMA PRÁCTICA
unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0
-}

-- 1.9 Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (m : ms) =
  if m < n
    then m : losMenoresA n ms
    else losMenoresA n ms

-- 1.10 Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (xs : xss) =
  if longitud xs > n
    then xs : lasDeLongitudMayorA n xss
    else lasDeLongitudMayorA n xss

-- 1.11 Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y = [y]
agregarAlFinal (x : xs) y = x : agregarAlFinal xs y

-- 1.12 Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los elementos de la segunda a continuación. Definida en Haskell como (++).
agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x : xs) ys = x : agregar xs ys

-- 1.13 Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = agregarAlFinal (reversa xs) x

-- 1.14 Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el elemento n de la primera lista y
-- de la segunda lista, teniendo en cuenta quelas listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _ = []
zipMaximos _ [] = []
zipMaximos (n : ns) (m : ms) = max n m : zipMaximos ns ms

-- 1.15 Dada una lista devuelve el mínimo
elMinimo :: (Ord a) => [a] -> a
elMinimo [x] = x
elMinimo (x : xs) =
  min x (elMinimo xs)

-- 2. Recursión sobre números
-- Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique lo contrario:

-- 2.1 Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
-- PRECOND.: n >= 0
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 2.2 Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n - 1)

-- 2.3 Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n - 1) e

-- 2.4 Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs. Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x : xs) = x : losPrimeros (n - 1) xs

-- 2.5 Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs
sinLosPrimeros _ [] = []
sinLosPrimeros n (x : xs) = sinLosPrimeros (n - 1) xs

-- 3. Registros
-- 3.1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:

data Persona = P String Int -- Nombre, edad
  deriving (Show)

-- Aux.: Funciones observadoras:
edad :: Persona -> Int
edad (P _ e) = e

-- 3.1.a Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (p : ps) =
  if n < edad p
    then p : mayoresA n ps
    else mayoresA n ps

-- 3.1.b Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
promedioEdad :: [Persona] -> Int
-- Precondición: la lista al menos posee una persona.
promedioEdad ps = edadDeLasPersonas ps `div` longitud ps

-- Aux.: Dada una lista de personas, devuelve una lista solo con las edades
edadDeLasPersonas :: [Persona] -> Int
edadDeLasPersonas [p] = edad p
edadDeLasPersonas (p : ps) = edad p + edadDeLasPersonas ps

-- 3.1.c Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la lista al menos posee una persona.
compararEdad :: Persona -> Persona -> Persona
compararEdad p1 p2 =
  if edad p1 > edad p2
    then p1
    else p2

elMasViejo :: [Persona] -> Persona
-- PRECOND: la lista al menos posee una persona.
elMasViejo [p] = p
elMasViejo (p : ps) = compararEdad p (elMasViejo ps)

-- 3.2. Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente manera:

data TipoDePokemon = Agua | Fuego | Planta
  deriving (Show)

data Pokemon = ConsPokemon TipoDePokemon Int
  deriving (Show)

data Entrenador = ConsEntrenador String [Pokemon]
  deriving (Show)

-- Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria. Definir en base a esa representación las siguientes funciones:

-- Entrenadores
pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (ConsEntrenador _ pks) = pks

-- 3.2.a Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ pks) = longitud pks

-- 3.2.b Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ pks) = cantPokemonDeTipo t pks

cantPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeTipo t [] = 0
cantPokemonDeTipo t (pk : pks) = unoSi (sonDelMismoTipo t (tipo pk)) + cantPokemonDeTipo t pks

{- DEFINIDA EN LA MISMA PRÁCTICA
unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0
-}

sonDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipo Agua Agua = True
sonDelMismoTipo Fuego Fuego = True
sonDelMismoTipo Planta Planta = True
sonDelMismoTipo _ _ = False

-- Aux.: Función observadora el tipo del pokemon dado
tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t _) = t

-- 3.2.c Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al primer entrenador,
--       que le ganarían a todos los Pokemon del segundo entrenador.

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ tipo entrenador1 entrenador2 =
  contarGanadores (filtrarPorTipo tipo (todosLosPokemonesDe entrenador1)) (todosLosPokemonesDe entrenador2)

-- Aux.: Función observadora para obtener todos los pokemones de un entrenador
todosLosPokemonesDe :: Entrenador -> [Pokemon]
todosLosPokemonesDe (ConsEntrenador _ listaDePokemones) = listaDePokemones

-- Aux.: Indica si el pokemon dado es superior a todos los de la lista de pokemones dada
esSuperiorATodos :: Pokemon -> [Pokemon] -> Bool
esSuperiorATodos _ [] = True
esSuperiorATodos poke (pk : pks) = superaA poke pk && esSuperiorATodos poke pks

-- Aux.: Filtra los pokemones de la lista por el tipo dado
filtrarPorTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
filtrarPorTipo _ [] = []
filtrarPorTipo t (pk : pks) =
  if sonDelMismoTipo t (tipo pk)
    then pk : filtrarPorTipo t pks
    else filtrarPorTipo t pks

-- Cuenta la cantidad de Pokemones de la primera lista dada que le ganarían a todos los de la segunda.
contarGanadores :: [Pokemon] -> [Pokemon] -> Int
contarGanadores [] _ = 0
contarGanadores (p : ps) pksEntrenador2 = unoSi (esSuperiorATodos p pksEntrenador2) + contarGanadores ps pksEntrenador2

{- DEFINIDA EN LA MISMA PRÁCTICA
unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0
-}

-- Aux. práctica 1 : Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon tipo1 _) (ConsPokemon tipo2 _) = pokemonSuperior tipo1 tipo2

-- Aux.: práctica 1 - ejercicio 4.2.a
pokemonSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
pokemonSuperior Agua Fuego = True
pokemonSuperior Fuego Planta = True
pokemonSuperior Planta Agua = True
pokemonSuperior _ _ = False

-- 3.2.d Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = hayAlMenosUnoDeCadaUno (todosLosPokemonesDe e)

-- Aux.: Indica si en la lista dada existe al menos un pokemon de cada tipo.
hayAlMenosUnoDeCadaUno :: [Pokemon] -> Bool
hayAlMenosUnoDeCadaUno pokemones =
  (contieneTipo Agua pokemones && contieneTipo Fuego pokemones) && contieneTipo Planta pokemones

-- Aux.: Indica si el tipo a buscar existe en la lista dada. Por ejemplo, si el tipo es Fuego: Se busca en la lista dada si existe algún pokemon de ese tipo.
contieneTipo :: TipoDePokemon -> [Pokemon] -> Bool
contieneTipo _ [] = False
contieneTipo tipoABuscar (pk : pks) = sonDelMismoTipo tipoABuscar (tipo pk) || contieneTipo tipoABuscar pks

-- 3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro de una empresa de software,
--   junto al proyecto en el que se encuentran. Así, una empresa es una lista de personas con diferente rol. La definición es la siguiente:

data Seniority = Junior | SemiSenior | Senior
  deriving (Show)

data Proyecto = ConsProyecto String
  deriving (Show)

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
  deriving (Show)

data Empresa = ConsEmpresa [Rol]
  deriving (Show)

-- Definir las siguientes funciones sobre el tipo Empresa:

-- 3.1.a Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa roles) = proyectos' roles

proyectos' :: [Rol] -> [Proyecto]
proyectos' [] = []
proyectos' (r : rs) = agregarALaLista (proyecto r) (proyectos' rs)

agregarALaLista :: Proyecto -> [Proyecto] -> [Proyecto]
agregarALaLista p ps =
  if proyectoPerteneceALaLista p ps
    then p : ps
    else ps

proyectoPerteneceALaLista :: Proyecto -> [Proyecto] -> Bool
proyectoPerteneceALaLista p ps = pertenece (nombreProyecto p) (nombresDeLosProyectos ps)

proyecto :: Rol -> Proyecto
proyecto (Developer _ p) = p
proyecto (Management _ p) = p

-- Aux.: Lista todos los nombres de los proyectos de una lista dada
nombresDeLosProyectos :: [Proyecto] -> [String]
nombresDeLosProyectos [] = []
nombresDeLosProyectos (p : ps) = nombreProyecto p : nombresDeLosProyectos ps

-- Aux.: Función observadora para obtener el nombre de un proyecto
nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto nombre) = nombre

{- FUE DEFINIDA CON ANTERIORIDAD EN ESTA MISMA PRÁCTICA
  EJ.: 1.7 Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
  pertenece :: (Eq a) => a -> [a] -> Bool
  pertenece y [] = False
  pertenece y (x : xs) = y == x || pertenece y xs
-}

-- 3.1.b Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa roles) proyectos = losDevSenior' roles proyectos

-- Aux.: Denota la cantidad de dev seniors que trabajan en algun proyecto de la lista dada
losDevSenior' :: [Rol] -> [Proyecto] -> Int
losDevSenior' [] _ = 0
losDevSenior' _ [] = 0
losDevSenior' (rol : roles) proyectos = unoSi (esSeniorYTrabajaEnAlgunProyecto rol proyectos) + losDevSenior' roles proyectos

unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0

-- Aux.: Evalua si el proyecto al que está asignado un rol pertenece a la lista de los proyectos dada y si el mismo es DevSenior
esSeniorYTrabajaEnAlgunProyecto :: Rol -> [Proyecto] -> Bool
esSeniorYTrabajaEnAlgunProyecto rol proyectos = (nombreDelProyectoDe_ rol `pertenece` nombresDeLosProyectos proyectos) && esDevSenior rol

-- Aux.: Dado un rol denota el nombre del proyecto al que está asignado
nombreDelProyectoDe_ :: Rol -> String
nombreDelProyectoDe_ rol = nombreProyecto (proyecto rol)

-- Aux.: Con pattern matching evaluamos si el rol actual es Senior o no.
esDevSenior :: Rol -> Bool
esDevSenior (Developer Senior _) = True
esDevSenior _ = False

{-      >> PRUEBAS - EMPRESAS - DEFINICIONES
  -- >> PROYECTOS
  proyectoAlpha :: Proyecto
  proyectoAlpha = ConsProyecto "Alpha"

  proyectoBeta :: Proyecto
  proyectoBeta = ConsProyecto "Beta"

  proyectoGamma :: Proyecto
  proyectoGamma = ConsProyecto "Gamma"

  proyectoOmega :: Proyecto
  proyectoOmega = ConsProyecto "Omega"

  -- >> ROLES

  -- >> SENIOR DEVS

  rolSenior1 = Developer Senior proyectoAlpha

  rolSenior2 = Developer Senior proyectoBeta

  rolSenior3 = Developer Senior proyectoAlpha -- mismo proyecto que rolSenior1

  rolSenior4 = Developer Senior proyectoGamma

  rolSenior5 = Developer Senior proyectoOmega

  -- >> OTROS ROLES (no senior o no dev)
  rolJunior = Developer Junior proyectoAlpha

  rolSemiSenior = Developer SemiSenior proyectoBeta

  rolManagement1 = Management Senior proyectoGamma

  rolManagement2 = Management Junior proyectoOmega

  -- >> EMPRESAS

  empresaGrande :: Empresa
  empresaGrande =
    ConsEmpresa
      [ rolSenior1,
        rolSenior2,
        rolSenior3,
        rolSenior4,
        rolSenior5,
        rolJunior,
        rolSemiSenior,
        rolManagement1,
        rolManagement2
      ] -- Total: 5 dev seniors

  empresaSoloAlpha :: Empresa
  empresaSoloAlpha =
    ConsEmpresa
      [ rolSenior1,
        rolSenior3,
        rolJunior
      ] -- 2 dev seniors (ambos en Alpha)

  empresaSinSeniors :: Empresa
  empresaSinSeniors =
    ConsEmpresa
      [ rolJunior,
        rolSemiSenior,
        rolManagement1
      ] -- 0 dev seniors

  empresaSoloManagement :: Empresa
  empresaSoloManagement =
    ConsEmpresa
      [ rolManagement1,
        rolManagement2
      ] -- 0 dev seniors
-}

{-        >> PRUEBAS - Empresas

  ghci> losDevSenior empresaGrande [proyectoAlpha, proyectoBeta, proyectoGamma, proyectoOmega]
  >>> 5

  * proyectoAlpha: rolSenior1, rolSenior3 (2)
  * proyectoBeta: rolSenior2 (1)
  * proyectoGamma: rolSenior4 (1)
  * proyectoOmega: rolSenior5 (1)

  ghci> losDevSenior empresaGrande [proyectoAlpha, proyectoBeta]
  >>> 3

  * proyectoAlpha: rolSenior1, rolSenior3 (2)
  * proyectoBeta: rolSenior2 (1)

  losDevSenior empresaGrande [proyectoGamma]
  >>> 1

  * proyectoGamma: rolSenior4 (1)

  losDevSenior empresaSoloAlpha [proyectoAlpha, proyectoBeta]
  >>> 2

  * proyectoAlpha: rolSenior1, rolSenior3 (2)
  * proyectoBeta: ninguno

  losDevSenior empresaSinSeniors [proyectoAlpha, proyectoBeta, proyectoGamma]
  >>> 0

  * No hay dev seniors en esta empresa

  losDevSenior empresaSoloAlpha [proyectoAlpha]
  >>> 2

  * rolSenior1 y rolSenior3 trabajan en Alpha

  losDevSenior empresaSoloManagement [proyectoGamma]
  * rolManagement1 es Senior pero no Developer
-}

-- 3.1.c Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn proyectos (ConsEmpresa roles) = cantQueTrabajanEn' roles proyectos

cantQueTrabajanEn' :: [Rol] -> [Proyecto] -> Int
cantQueTrabajanEn' [] proyectos = 0
cantQueTrabajanEn' (r : rs) proyectos = unoSi (nombreDelProyectoDe_ r `pertenece` nombresDeLosProyectos proyectos) + cantQueTrabajanEn' rs proyectos

{- DEFINIDA EN LA MISMA PRÁCTICA
unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0
-}

-- 3.1.d Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto empresa = asignadosPorProyecto' (proyectos empresa) (todosLosRolesDeLaEmpresa empresa)

asignadosPorProyecto' :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
asignadosPorProyecto' [] _ = []
asignadosPorProyecto' (p : ps) roles = (p, contarPersonasEnProyecto p roles) : asignadosPorProyecto' ps roles

contarPersonasEnProyecto :: Proyecto -> [Rol] -> Int
contarPersonasEnProyecto _ [] = 0
contarPersonasEnProyecto p (r : rs) = unoSi (trabajaEnElProyecto r p) + contarPersonasEnProyecto p rs

trabajaEnElProyecto :: Rol -> Proyecto -> Bool
trabajaEnElProyecto r p = nombreProyecto (proyecto r) == nombreProyecto p

{- DEFINIDA EN LA MISMA PRÁCTICA
unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0
-}

{- DEFINIDA CON ANTERIORIDAD EN ESTA PRÁCTICA
proyectoDelRol :: Rol -> Proyecto
proyectoDelRol (Developer _ p) = p
proyectoDelRol (Management _ p) = p
-}

todosLosRolesDeLaEmpresa :: Empresa -> [Rol]
todosLosRolesDeLaEmpresa (ConsEmpresa roles) = roles
