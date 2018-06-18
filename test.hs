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
         

caminoTuplas :: [Desplazamiento] -> [(Integer,Integer)]
caminoTuplas a = ((1,1):caminoTuplasAux 1 1 a) 

caminoTuplasAux :: Integer -> Integer -> [Desplazamiento] -> [(Integer,Integer)]
caminoTuplasAux i j [] = []
caminoTuplasAux i j (Derecha:xs) = (i,j+1) : caminoTuplasAux i (j+1) xs 
caminoTuplasAux i j (Izquierda:xs) = (i,j-1) : caminoTuplasAux i (j-1) xs
caminoTuplasAux i j (Abajo:xs) = (i+1,j) : caminoTuplasAux (i+1) j xs
caminoTuplasAux i j (Arriba:xs) = (i-1,j) : caminoTuplasAux (i-1) j xs


caminoValido :: Tablero a -> Camino -> Bool
caminoValido a b = caminoValidoAux a (caminoTuplas b) 


caminoValidoAux :: Tablero a -> [(Integer,Integer)] -> Bool
caminoValidoAux a [] = True
caminoValidoAux a (x:xs) = posValida a x && caminoValidoAux a xs 


caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida a b = caminoValido a b && caminoDeSalidaAux a (caminoTuplas b)

caminoDeSalidaAux :: CampoMinado -> [(Integer,Integer)] -> Bool
caminoDeSalidaAux a (x:[]) = not(valor a x) && x == (tamano a,tamano a) 
caminoDeSalidaAux a (x:xs) = not(valor a x) && caminoDeSalidaAux a xs

caminoDeSalidaSinRepetidos :: CampoMinado -> Camino -> Bool
caminoDeSalidaSinRepetidos a b = noHayRepetidos (caminoTuplas b) && caminoDeSalida a b 

noHayRepetidos :: [(Integer,Integer)] -> Bool
noHayRepetidos (x:[]) = True
noHayRepetidos (x:xs) = not(pertenece x xs) && noHayRepetidos xs

pertenece :: (Integer,Integer) -> [(Integer,Integer)] -> Bool
pertenece e [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs
                   
agregarATodas :: Integer -> [[Integer]] -> [[Integer]] 
agregarATodas a [] = []
agregarATodas a (x:xs) = (a:x):(agregarATodas a xs)
