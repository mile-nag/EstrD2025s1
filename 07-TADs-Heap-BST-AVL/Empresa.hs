-- Desactiva todas las advertencias
{-# OPTIONS_GHC -w #-}

module Empresa
  ( Empresa,
    consEmpresa,
    buscarPorCUIL,
    empleadosDelSector,
    todosLosCUIL,
    todosLosSectores,
    agregarSector,
    agregarEmpleado,
    agregarASector,
    borrarEmpleado,
  )
where

import MapV2 (emptyM)
import Set qualified (dummy)
import SetV2 (addS, setToList)

{-
Donde se observa que:
- los empleados son un tipo abstracto.
- el primer map relaciona id de sectores con los empleados que trabajan en dicho sector.
- el segundo map relaciona empleados con su número de CUIL.
- un empleado puede estar asignado a más de un sector
- tanto Map como Set exponen una interfaz eficiente con costos logarítmicos para inserción, búsqueda y borrado, tal cual vimos en clase
-}

-- import MapBST
-- import SetBST
-- import Empleado

type SectorId = Int

type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

{- INV. REP.: Sea ConsE ms me
   * Todos los empleados que existan en ms deben existir también en me
   * En me debe coincidir el cuil del emleado con en empleado asociado
   OBERVACIONES:
   * todos los sectores a los que puede pertenecer un empeado existen en me
   * el cuil debe ser un número válido
   * un empleado puede estar asignado a más de un sector
-}

{-
-- TAD EMPLEADO
    -- Propósito: construye un empleado con dicho CUIL.
    consEmpleado :: CUIL -> Empleado                            -- Costo: O(1)

    -- Propósito: indica el CUIL de un empleado.
    cuil :: Empleado -> CUIL                                    -- Costo: O(1)

    -- Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
    incorporarSector :: SectorId -> Empleado -> Empleado        -- Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.

    -- Propósito: indica los sectores en los que el empleado trabaja.
    sectores :: Empleado -> [SectorId]                          -- Costo: O(S)
-}

-- Implementar las siguientes funciones: (En los costos, S es la cantidad de sectores de la empresa, y E es lacantidad de empleados.)

-- Propósito: construye una empresa vacía.
-- Costo: O(1)
consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM

{- Justificación de costos:
   - ConsE emptyM emptyM son de orden O(1) porque no dependen de ningun dato
-}

-- Propósito: devuelve el empleado con dicho CUIL.
-- Precondición: el CUIL es de un empleado de la empresa.
-- Costo: O(log E)
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE _ me) = fromJust (lookupM c me)

{- Justificación de costos:
   - fromJust O(1)
   - lookupM O(log E) siendo E el total de empleados de la empresa porque se busca en el map que contiene a las asociasiones CUIL-Empleado
   => El costo de la función es de O(log E), O(1) al tener menor impacto en el costo final, se desestima
-}

-- Propósito: indica los empleados que trabajan en un sector dado.
-- Costo: O(log S + E)
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector si (ConsE ms _) = case lookupM si ms of
  Just set -> setToList set
  Nothing -> error "No existe el sector en la empresa"

{- Justificación de costos:
   - lookupM O(log K) siendo K todas las claves del map, como en este caso las claves son todos los sectores de la empresa, es de orden S
             O(log S)
   - case ... of O(1)
   - En caso de encontrar el set de empleados asociado al sector, se realiza un setToList de orden O(e) siendo e todos los empleados del sector
   - En peor caso, todos los empleados pertenecen al sector siendo e = E
   => El costo final de la función es O(log S + E)
-}

-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E)
todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE _ me) = keys me

{- Justificación de costos:
   - Se realiza la operación keys me que es de orden E siendo E todos los empleados de la empresa -> O(E)
   => El costo final de la función es O(E)
-}

-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S)
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE ms _) = keys ms

{- Justificación de costos:
   - Se realiza la operación keys me que es de orden S siendo S todos los sectores de la empresa -> O(S)
   => El costo final de la función es O(S)
-}

-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(log S)
agregarSector :: SectorId -> Empresa -> Empresa
agregarSector sid (ConsE ms _) = case lookupM sid ms of
  Just id -> error "El sector ya se encuentra en la empresa"
  Nothing -> ConsE (assocM sid emptyS ms)

{- Justificación de costos:
   * Sea E la cantidad de empleados de la empresa
   * Sea S la cantidad total de sectores en la empresa

   - lookupM sid ms -> opera sobre el map de sectores de la empresa, el costo es O(log S)
   - En caso de que el empleado no exista en la empresa, se devuelve la empresa "rearmada" con la nueva asociación SectorId-SetEmpleado(vacío)
     esto tiene un costo de O(log S) ya que opera sobre los sectores de la empresa
   => El costo final de esta función es de O(log S + log S) = O(2 log S) = O(log S)
-}

-- data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

-- Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el
-- CUIL dado.
-- Costo: calcular.
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado sids c (ConsE ms me) =
  let nuevoEmpleado = incorporarSectores sids (consEmpleado c)
      nuevoMs = agregarEmpleadoASectores sids nuevoEmpleado ms
      nuevoMe = assocM c nuevoEmpleado me
   in ConsE nuevoMs nuevoMe

incorporarSectores :: [SectorId] -> Empleado -> Empleado
incorporarSectores [] e = e
incorporarSectores (si : sis) e = incorporarSector si (incorporarSectores sis e)

agregarEmpleadoASectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmpleadoASectores [] e ms = ms
agregarEmpleadoASectores (si : sis) e ms = case lookupM si ms of
  Just set -> assocM si (addS e set) (agregarEmpleadoASectores sis e ms)
  Nothing -> error "Uno de los sectores no pertenece a la empresa"

{- Justificación de costos:
   - Función auxiliar agregarEmpleadoASectores:
     * Realiza una recursion sobre la lista [SectoId] pasada como argumento, esta tiene cantidad N de elementos
     * En cada paso de la recursión realisa un case...of O(1), un lookupM si ms O(log S) siendo S la cantidad de sectores de la empresa
       y en caso de encontrar el valor asociado realiza un assocM O(log S) siendo S la cantidad de sectores de la empresa y un
       addS para hacer la agregar el empleado al Set Empleado correspondiente al sector con un costo O(log e) siendo e todos los empleados del sector
      -> cada paso tiene un costo O(1) + O(log S) + O(log e) y esto se realiza N veces
      -> En peor caso, los empleados del sector (e) son todos los empleados de la empresa (E)
      => Entonces, el coste operacional de la función auxiliar es de O(N * (log S + log E + 1)) = O(N * (log S + log E))
   - Función auxiliar incorporarSectores:
     * Realiza una recursión sobre la lista [SectorId] dada como argumento con N elementos.
     * En cada paso de la recursión realiza incorporarSector (del TAD empleado) con un costo de O(log s) siendo s la cantidad de sectores
       a los que está asignado el empleado, pero en peor caso s = S (todos los sectores de la empresa)
      => El costo operacional de la función auxiliar es de O(log S) - en peor caso y esto se realiza N veces: O(N * log S)
   - La función principal agregarEmpleado:
     * se establecen 3 variables para rearmar la empresa en el resultado final:
       - nuevoEmpleado = incorporarSectores sids (consEmpleado c) -- O(N log S) + O(1) = O(N log S)
       - nuevoMs = agregarEmpleadoASectores sids nuevoEmpleado ms -- O((N log S) + (N log E))
       - nuevoMe = assocM c nuevoEmpleado me                      -- O(log E) siendo E la cantidad de empleados de la empresa en ms
      => El costo final de la operación es de: O(N log S) + O((N log S) + (N log E)) + O(log E)
                                                     O(N log S) + O(N log E) + O(log E)
                                                           O(N log S) + O(N log E) => O(N * (log S + log E))
-}

-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: calcular
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector sid c (ConsE ms me) =
  case lookupM c me of
    Just empleado ->
      let nuevoE = incorporarSector sid empleado
          nuevoMs = case lookupM sid ms of
            Just set -> assocM sid (addS nuevoE set) ms
            Nothing -> error "El sector no pertenece a la empresa"
          nuevoMe = assocM c nuevoE me
       in ConsE nuevoMs nuevoMe
    Nothing -> error "El empleado no pertenece a la empresa"

{- Justificación de costos:
   - lookupM c me: O(log E), donde E es la cantidad de empleados en la empresa
   - incorporarSector sid empleado: O(log s), donde s es la cantidad de sectores del empleado (en peor caso, s = S)
   - lookupM sid ms: O(log S), donde S es la cantidad de sectores de la empresa.
   - addS nuevoE set: O(log e), donde e es la cantidad de empleados en el sector (en peor caso, e = E)
   - assocM sid ... ms y assocM c ... me: O(log S) y O(log E)
   => Costo total: O(log E + log S + log E) = O(log S + log E).
-}