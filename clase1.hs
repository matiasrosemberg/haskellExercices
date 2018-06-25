data Desplazamiento = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)

type Conjunto a = [a]
type Camino = [Desplazamiento]
type Posicion = (Integer,Integer)
type Tablero a = [[a]]
type CampoMinado = Tablero Bool
type TableroAF = Tablero Desplazamiento

-- Devuelve el tamaño de un tablero.
tamano :: Tablero a -> Integer
tamano t = fromIntegral(length t)

-- Devuelve el valor de una posición de un tablero.
-- Notar que la primera posición de arriba a la izquierda es la (1,1).
valor :: Tablero a -> Posicion -> a
valor t (i,j) = iesimo (iesimo t i) j

-- Devuelve el iésimo elemento de una lista. El primer elemento es el 1.
iesimo :: [a] -> Integer -> a
iesimo (x:xs) 1 = x
iesimo (x:xs) n = iesimo xs (n-1)

-- Determina si una posición está dentro de los límites de un tablero.
posValida :: Tablero a -> Posicion -> Bool
posValida t (i,j) = 1<=i && i<=n && 1<=j && j<=n
    where n = tamano t
    
    
-- Funciones de ejemplo, solo para ilustrar cómo usar los tipos definidos arriba.
-- Determina si un desplazamiento es vertical (Arriba o Abajo).
esVertical :: Desplazamiento -> Bool
esVertical Arriba = True
esVertical Abajo = True
esVertical _ = False

-- Cuenta la cantidad de Desplazamientos verticales en un Camino.
contarDesplazamientosVerticales :: Camino -> Integer
contarDesplazamientosVerticales [] = 0
contarDesplazamientosVerticales (x:xs) | esVertical x = 1 + resto
                                       | otherwise    = resto
  where resto = contarDesplazamientosVerticales xs

-- Caminos de prueba.
camino1 = [Derecha, Abajo, Izquierda, Arriba, Abajo, Abajo, Derecha, Derecha]
camino2 = [Derecha, Abajo, Derecha, Abajo]
camino3 = [Derecha, Abajo, Derecha, Izquierda, Derecha, Abajo]
camino4 = [Derecha,Derecha,Derecha]

-- CampoMinado de prueba.
campo1 :: CampoMinado
campo1 = [ [False, False, True],
           [True,  False, False],
           [True,  True,  False] ]

-- TableroAF de prueba, sin ciclos.
taf1 :: TableroAF
taf1 = [ [Derecha,  Derecha, Abajo],
         [Arriba, Izquierda, Abajo],
         [Arriba, Izquierda, Abajo] ]

-- TableroAF de prueba, con ciclos.
taf2 :: TableroAF
taf2 = [ [Derecha,       Abajo, Abajo],
         [Arriba,    Izquierda, Abajo],
         [Izquierda, Izquierda, Izquierda] ]
         
-- Dado un Camino, devolvemos sus Posiciones desde el principio hasta el fin.
caminoTuplas :: Camino -> [Posicion]
caminoTuplas a = ((1,1):caminoTuplasAux 1 1 a) 

-- Traduce desplazamientos a expresiones vectoriales correspondientes a las posiciones del tablero en base a sus coordenadas.
caminoTuplasAux :: Integer -> Integer -> Camino -> [Posicion]
caminoTuplasAux i j [] = []
caminoTuplasAux i j (Derecha:xs) = (i,j+1) : caminoTuplasAux i (j+1) xs 
caminoTuplasAux i j (Izquierda:xs) = (i,j-1) : caminoTuplasAux i (j-1) xs
caminoTuplasAux i j (Abajo:xs) = (i+1,j) : caminoTuplasAux (i+1) j xs
caminoTuplasAux i j (Arriba:xs) = (i-1,j) : caminoTuplasAux (i-1) j xs


caminoValido :: Tablero a -> Camino -> Bool
caminoValido a b = caminoValidoAux a (caminoTuplas b) 

-- Si todas las posiciones de un camino son validas, el camino es valido. (Este modelo no permite salir y volver a entrar)
caminoValidoAux :: Tablero a -> [Posicion] -> Bool
caminoValidoAux a [] = True
caminoValidoAux a (x:xs) = posValida a x && caminoValidoAux a xs 

caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida a b = caminoValido a b && caminoSinMinas a (caminoTuplas b) 

-- Verifico que dado una lista de posiciones llega a la posicion final sin pasar por ninguna mina.
caminoSinMinas :: CampoMinado -> [Posicion] -> Bool
caminoSinMinas a (x:[]) = not(valor a x) && x == (tamano a,tamano a) 
caminoSinMinas a (x:xs) = not(valor a x) && caminoSinMinas a xs

caminoDeSalidaSinRepetidos :: CampoMinado -> Camino -> Bool
caminoDeSalidaSinRepetidos a b = noHayRepetidos (caminoTuplas b) && caminoDeSalida a b 

-- Verifico que dada una lista de posiciones no haya repetidos entre ellas.
noHayRepetidos :: [Posicion] -> Bool
noHayRepetidos (x:[]) = True
noHayRepetidos (x:xs) = not(pertenece x xs) && noHayRepetidos xs

-- Verifico que una posicion no se repita dentro de una lista de posiciones
pertenece :: Posicion -> [Posicion] -> Bool
pertenece e [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs
                   
-- Funcion programada en clase, agrega un elemento a una lista de listas                   
agregarATodas :: a -> [[a]] -> [[a]] 
agregarATodas a [] = []
agregarATodas a (x:xs) = (x++[a]):(agregarATodas a xs)

-- Genera todos los caminos de k desplazamientos para todas las direcciones desde el origen (1,1)
-- Incluyendo aquellos que no son validos.
generarDezplazamientos :: Integer -> [Camino] 
generarDezplazamientos 0 = []
generarDezplazamientos 1 = [[Derecha],[Abajo]]
generarDezplazamientos k = agregarATodas Derecha (generarDezplazamientos (k-1)) ++ agregarATodas Izquierda (generarDezplazamientos (k-1)) ++ agregarATodas Arriba (generarDezplazamientos (k-1)) ++ agregarATodas Abajo (generarDezplazamientos (k-1))

-- Toma todos los caminos posibles de k desplazamientos y filtra los caminos validos.
salidasEnKDesp :: CampoMinado -> Integer -> [Camino]
salidasEnKDesp a b = salidasEnKDespAux a (generarDezplazamientos b)

-- Para cada camino de llegada devuelvo los que son caminos validos.
salidasEnKDespAux :: CampoMinado -> [Camino] -> [Camino]
salidasEnKDespAux a [] = []
salidasEnKDespAux a (x:xs) | (caminoDeSalida a x) = (x : salidasEnKDespAux a xs) 
                                   | otherwise = salidasEnKDespAux a xs

recorrido :: TableroAF -> Posicion -> [Posicion]
recorrido a x | (posValida a x) = x : recorrido a (siguientePosicion a x)
              | otherwise = []

-- Indica la siguiente posicion a la que se debe desplazar el robot.
siguientePosicion :: TableroAF -> Posicion -> Posicion
siguientePosicion a (i,j) | valor a (i,j) == Derecha = (i,j+1)
                          | valor a (i,j) == Izquierda = (i,j-1)
                          | valor a (i,j) == Abajo = (i+1,j)
                          | otherwise = (i-1,j)
                          
escapaDelTablero :: TableroAF -> Posicion -> Bool
escapaDelTablero a x = escapaDelTableroAux a x []

-- Verifico mientras se genera el recorrido , si se da un loop o si logro salir del tablero.
-- Si se repite una posicion, se da un loop.
-- Si llego a una posicion que no es valida, salgo del tablero.
escapaDelTableroAux :: TableroAF -> Posicion -> [Posicion] -> Bool
escapaDelTableroAux a x xs | posValida a x && not(pertenece x xs) = escapaDelTableroAux a (siguientePosicion a x) (x:xs)   
                           | posValida a x && (pertenece x xs) = False
                           | otherwise = True
                           
-- Nuevo valor rotado en sentido horario.
rotar :: Desplazamiento -> Desplazamiento
rotar Derecha = Abajo
rotar Abajo = Izquierda
rotar Izquierda = Arriba
rotar Arriba = Derecha

-- Refresca el tablero dinamico en base a la siguiente posicion del robot.
-- Busca la fila que quiero modificar
siguienteTablero :: TableroAF -> Posicion -> TableroAF
siguienteTablero (x:xs) (1,j) = (modificarLista x j) : xs 
siguienteTablero (x:xs) (i,j) = x : siguienteTablero xs ((i-1),j)

-- Busca la columna que quiero modificar
modificarLista :: [Desplazamiento] -> Integer -> [Desplazamiento]
modificarLista (x:xs) 1 = (rotar x) : xs
modificarLista (x:xs) j = x : (modificarLista xs (j-1))

cantidadDePasosParaSalir :: TableroAF -> Posicion -> Integer
cantidadDePasosParaSalir a p | posValida a p = 1 + cantidadDePasosParaSalir (siguienteTablero a p) (siguientePosicion a p)
                             | otherwise = 0
