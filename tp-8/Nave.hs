import ...

{-
>> Nave Espacial
   En este examen modelaremos una Nave como un tipo abstracto, el cual nos permite construir una nave espacial, dividida en
   sectores, a los cuales podemos asignar tripulantes y componentes. Para esto, damos por hecho que:
-}

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]

data Barril = Comida | Oxigeno | Torpedo | Combustible

--  El tipo Sector es un tipo abstracto, y representa al sector de una nave, el cual contiene componentes y tripulantes asignados.
--  El tipo Tripulante es un tipo abstracto, y representa a un tripulante dentro de la nave, el cual tiene un nombre, un rangoy sectores asignados.
--  El tipo SectorId es sinónimo de String, e identifica al sector de forma unívoca.
--  Los tipos Nombre y Rango son sinónimos de String. Todos los nombres de tripulantes son únicos.
--  Un sector está vacío cuando no tiene tripulantes, y la nave está vacía si no tiene ningún tripulante.
--  Puede haber tripulantes sin sectores asignados.

-- >> Representación
-- Dicho esto, la representación será la siguiente (que no es posible modificar ):

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

-- Esta representación utiliza:
-- Un Map que relaciona para cada SectorId su sector correspondiente.
-- Otro Map que relaciona para cada Nombre de tripulante el tripulante con dicho nombre.
-- Una MaxHeap que incluye a todos los tripulantes de la nave, cuyo criterio de ordenado es por rango de los tripulantes.


-- 1. Dar invariantes de representación válidos según la descripción de la estructura.

{- Inv. Rep.: en N ms mt mht
    * En la mht no tiene tripulantes repetidos-
    * todos los tripulantes que estan en mt están también en mht y viceversa --> Acá hay que aclarar que son : claves o valores y como aparecen, POR ESO hay que hacer dos invariantes
    * Para todo par (clave, tripulante) en mapTripulantes, se cumple que nombreT tripulante == clave
    * Todos los tripulantes que el sector en ms tiene asignados, también estan en mt y mht
    * Todos los sectores que el tripulante tiene asignados, también están en ms -
-}

-- 2. Implementar la siguiente interfaz de Nave, utilizando la representación y los costos dados, calculando los costos de cada subtarea, y siendo T la cantidad de tripulantes y S la cantidad de sectores:

-- Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- Eficiencia: O(S) -- sLOG S
construir :: [SectorId] -> Nave
construir s = N (agregarSectoresA s emptyM) emptyM emptyH

agregarSectoresA :: [SectorId] -> Map SectorId Sector -> Map SectorId Sector 
agregarSectoresA [] map = map
agregarSectoresA (si:sis) map = assocM si (crearS si) (agregarSectoresA sis map)

-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T)
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N ms mt ht) = case lookupM n mt of
                              Just t -> error "El tripulante ya existe en la nave."
                              Nothing -> let t = crearT n 
                                          in N ms (assocM n t mt) (insertH t ht) 

{- Justificación de costos: 
   - crearT -> O(1)
   - assocM n t mt -> O(log T) siendo T todos los tripulantes de la nave, es de orden T porque opera sobre el map de tripulantes
   - insertH t ht -> O(log M) siendo M la cantidad de elementos de la heap, como es la misma cantidad que tripulantes de la nave y los ordena, es de orden O(log T)

   Costo final de la función: O(log T) + O(log T) = O(2log T) = O(log T)
-}

-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log M)
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N ms mt ht) = sectoresT (fromJust(lookupM n mt))

{- Justificación de costos:
   - lookupM n mt -> O(log T) siendo T el total de tripulantes del map 
   - sectoresT ... -> O(1) devuelve un set de sectores id a los que pertenece el tripulante
   - fromJust ... -> O(1) se puede utilizar porque la precondicion me asegura que el tripulante existe en la nave 

   => Costo final de la función: O(log T)
-}

-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
-- Eficiencia: O(log S)
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector sid (N ms mt ht) = let sector = fromjust(lookupM sid ms) 
                                 in ((tripulanteS sector), (componentesS sector))

-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-- Eficiencia: O(log T) => EL COSTO ESTA MAL EN LA PRÁCTICA
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N _ _ ht) = tripulantesN' ht

tripulantesN' :: MaxHeap Tripulante -> [Tripulante]
tripulantesN' ht = if isEmptyH ht 
                    then []
                    else maxH ht : tripulantesN (deleteMax ht)

{- Justificación de costos: * Siendo T la cantidad de tripulantes de la nave 
   En la auxiliar se realizan: 
    isEmptyH   O(1)
    maxH       O(1)
    deleteMax  O(log M) -> siendo M la cantidad de elementos en la heap, pero
                           este caso opera sobre la MaxHeap de tripulantes, entonces el costo es O(log T)
   - Esto se realiza por todos los elementos de la heap, que son T elementos
   - La función principal realiza un llamado a la función auxiliar
   => Costo final: O(T log T)
-}

-- Propósito: Asigna una lista de componentes a un sector de la nave.
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sid (N ms mt ht) = N (agregarComponentes cs sid ms) mt ht

agregarComponentes :: [Componente] -> SectorId -> Map SectorId Sector -> Map SectorId Sector
agregarComponentes cs sid map = let nuevoSector = case lookupM sid map of 
                                                   Just sector -> sector 
                                                   Nothing  -> error "El sector no existe en la nave."
                                    in assocM sid (agregarComponentes' cs sector) map 

agregarComponentes :: [Componentes] -> Sector -> Sector 
agregarComponentes [] s = s
agregarComponentes (c:cs) = agregarC c (agregarComponentes' cs s)

{- Justificación de costos:
   - La función auxiliar agregarComponentes' realiza una recursión sobre la lista de componentes pasada como argumento
     - Se realiza una operación constante O(1) (agregarC c) en cada paso de la recursión
     => El costo total de la función auxiliar es de O(C) siendo C la cantidad de elementos que contiene la lista de componentes
   - La función auxiliar agregarComponentes realiza un lookupM en busca de el sector asociado al SectorId pasado como argumento
     - El costo de esta búsqueda es O(log S) siendo S la cantidad de sectores en el map que contiene las asociaciones SectorId-Sector
     - En caso de encontrarlo, se realiza un assocM O(log S) asociando el SectorId al nuevo sector con los componentes actualizados. Este sector se actualiza
       con el llamado a la función auxiliar agregarComponentes' que tiene un costo de O(C) 
     => El costo final de esta función auxiliar es de O(log S + log S + C) = O(log S + C)
   - La función principal realiza un llamado a la función auxiliar agregarComponentes con un costo de O(log S + C)
   => El costo final es de O(log S + C)
-}

-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
-- Eficiencia: O(log S + log T + T log T)

-- Sea S los sectores de la nave
-- Sea T los tripulantes de la nave
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n sid (N ms mt ht) = let s = fromJust(lookupM sid ms)                    -- O(1) + O(log S)      -- fromJust + lookupM (map sectores)
                                        t = fromJust(lookupM n mt) 
                                        nuevoT = asignarS sid t                         -- O(1) + O(log T)      -- fromJust + lookupM (map tripulantes)
                                    in N (assocM sid (agregarT n s) ms)                 -- O(log S) + O(log T)  -- assocM ms +  agregarT s
                                         (assocM n nuevoT mt)                           -- O(log T) + O(log S)  -- assocM mt +  asignarS t
                                         (insertH nuevoT (actualizarHeap t ht))         -- O(T log T)           -- auxiliar: actualizarHeap
                                                                                        --  O(log S + log T + log S + log T + log T + log S + T log T)
                                                                                        -- O(3 log S + 3 log T + T log T)
                                                                                        -- O(log S + log T + T log T) 
                                                                            -- -> T log T crece más rapidamente con respecto a T, entonces, log T se absorbe en el costo final
                                                                         -- Costo final => O(log S + T log T)

-- Tengo que actualizar la heap porque tengo que mantener al tripulante sincronizado con los cambios 

actualizarHeap :: Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante
actualizarHeap t ht = if isEmptyH ht                                                   -- O(1)           isEmptyH
                      then maxH ht == t                                                -- O(1) + O(1)    maxH ht == t  
                            then actualizarHeap t (deleteMaxH ht)                      -- O(T)        -> recursión
                            else insertH (maxH ht) (actualizarHeap t (deleteMaxH ht))  -- O(log T)    -> siendo T los tripulantes de la heap 
                                                                       -- => Costo final: O(T log T)

-- 3. Implementar las siguientes funciones como usuario del tipo Nave, indicando la eficiencia obtenida para cada operación:

-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores :: Nave -> Set SectorId
sectores n = sectores' (tripulantesN n) -- O(log T)

sectores' :: Nave -> [Tripulante] -> Set SectorId 
sectores' [] = emptyS
sectores' (t:ts) = unionS (sectoresT t) (sectores' ts)

{- Justificación de costos: 
    * sea T la cantidad total de tripulantes
    * sea S la cantidad total de sectores
  - La función auxiliar sectores':
    - realiza una recursión sobre la lista [Tripulante], que tiene T elementos.
    - en cada paso:
      * sectoresT t -> acceso directo a un campo del tripulante -> O(1)
      * unionS ... -> en el peor caso O(S log S), si ambos sets tienen hasta S elementos.
    - entonces, cada paso cuesta O(S log S) + O(1) = O(S log S)
     -> como la recursión se realiza T veces, el costo total de sectores' es:  O(T * S log S)
  - la función sectores llama a tripulantesN, que tiene costo O(T), porque extrae todos los tripulantes de la MaxHeap.
  => Costo total de sectores:
      O(T + T * S log S) ≡ O(T * S log S)
-}

-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados :: Nave -> [Tripulante]
sinSectoresAsignados n = sinSectoresAsignados' (tripulantesN n)

sinSectoresAsignados' :: [Tripulante] -> [Tripulante]
sinSectoresAsignados' [] = []
sinSectoresAsignados' (t:ts) =
  if sizeS (sectoresT t) == 0
    then t : sinSectoresAsignados' ts
    else sinSectoresAsignados' ts

{- Justificación de costos: 
   - La función auxiliar realiza una recursion sobre la lista de tripulantes pasada como argumento 
     la lista tiene T elementos, por cada elemento se busca si la cantidad de sectores a los que esta asignado es 0 -> O(1)
     sectoresT que devuelve el set de sectores del tripulante cuesta O(1)
    -> Se realiza una operación constante por cada elemento de la lista, el costo es O(T) siendo T los tripulantes de la lista, en peor caso ninguno esta asignado a ningun sector.
   - La función principal realiza dos operaciones secuenciales e independientes:
     sinSectoresAsignados' = O(T)
     tripulantesN n = O(log T)
     -> O(T + log T), como T crece más rapidamente con respecto a T, el otro término es absorbido y el costo final es O(T)
   => El costo final de la operacion es O(T)
-}

-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles :: Nave -> [Barril]
barriles n = barriles' (datosDeLosSectores (setToList (sectores' (tripulantesN n))) n)

barriles' :: [Componente] -> [Barril]
barriles' [] = []
barriles' (c:cs) = case c of
                     Almacen b -> b ++ barriles' cs
                     _ -> barriles' cs

datosDeLosSectores :: [SectorId] -> Nave -> [Componente]
datosDeLosSectores [] _ = []
datosDeLosSectores (s:ss) n = let (_, cs) = datosDeSector s n
                              in cs ++ datosDeLosSectores ss n

sectores' :: [Tripulante] -> Set SectorId
sectores' [] = emptyS
sectores' (t:ts) = unionS (sectoresT t) (sectores' ts)
