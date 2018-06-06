type Set a = [a]

vacio :: Set Integer
vacio = []

agregar :: Integer -> Set Integer -> Set Integer
agregar n xs | elem n xs = xs
             | otherwise = n:xs

incluido :: Set Integer -> Set Integer -> Bool
incluido [] _ = True
incluido (x:xs) yx = (elem x yx) && incluido xs yx

iguales ::  Set Integer -> Set Integer -> Bool
iguales a b = incluido a b && incluido b a && (length a == length b)

agregarATodas :: Integer -> [[Integer]] -> [[Integer]] 
agregarATodas a [] = []
agregarATodas a (x:xs) = (a:x):(agregarATodas a xs)

partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = (agregarATodas n (partes (n-1)))++partes (n-1)

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano a b = productoCartesianoAux a a b


productoCartesianoAux :: Set Integer -> Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesianoAux _ _ [] = []
productoCartesianoAux [] xx (y:ys) = productoCartesianoAux xx xx ys
productoCartesianoAux (x:xs) xx (y:ys) = (x,y):productoCartesianoAux xs xx (y:ys)

variaciones :: Set Integer -> Integer -> [[Integer]]
variaciones [] 1 = []
variaciones (x:xs) 1 = [x]:variaciones xs 1
variaciones a b = agregarCadaElementoATodos a (variaciones a (b-1))

agregarCadaElementoATodos :: [Integer] -> [[Integer]] -> [[Integer]]
agregarCadaElementoATodos a b = agregarCadaElementoATodosAux a b b

agregarCadaElementoATodosAux :: [Integer] -> [[Integer]] -> [[Integer]] -> [[Integer]]
agregarCadaElementoATodosAux [] _ _ = []
agregarCadaElementoATodosAux (x:xs) yy [] = agregarCadaElementoATodosAux (xs) yy yy
agregarCadaElementoATodosAux (x:xs) yy (y:ys) = (x:y):agregarCadaElementoATodosAux (x:xs) yy ys

insertarEn :: [Integer] -> Integer -> Integer -> [Integer]
insertarEn lista elemento 1 = elemento:lista
insertarEn (x:xs) elemento n = x:insertarEn xs elemento (n-1)

insertarEnTodasLasPosiciones :: [Integer] -> Integer -> Integer -> [[Integer]]
insertarEnTodasLasPosiciones _ _ 0 = []
insertarEnTodasLasPosiciones xs elem len = insertarEn xs elem len : insertarEnTodasLasPosiciones xs elem (len-1)

insertarEnTodasLasPosicionesDeTodos :: [[Integer]] -> Integer -> [[Integer]]
insertarEnTodasLasPosicionesDeTodos (x:[]) elem = insertarEnTodasLasPosiciones x elem (fromIntegral(length x)+1)
insertarEnTodasLasPosicionesDeTodos (x:xs) elem = insertarEnTodasLasPosiciones x elem (fromIntegral(length x)+1) ++ insertarEnTodasLasPosicionesDeTodos xs elem

permutaciones :: Integer -> [[Integer]]
permutaciones 1 = [[1]]
permutaciones n = insertarEnTodasLasPosicionesDeTodos (permutaciones (n-1)) n 

nBolitaskCajas :: Integer -> Integer -> [[Integer]]
nBolitaskCajas n k = variaciones [1..k] n

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)

anotherMcd :: Integer -> Integer -> Integer
anotherMcd a 1 = 1
anotherMcd 1 b = 1
anotherMcd a b | testA == testB = testA * anotherMcd (div a testA) (div b testA)
               | testA > testB = anotherMcd a (div b testB)
               | otherwise = anotherMcd (div a testA) b
               where testA = menorDivisor a
                     testB = menorDivisor b

menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = auxMenorDivisor n 2

auxMenorDivisor :: Integer -> Integer -> Integer
auxMenorDivisor n m | mod n m == 0 = m
                    | otherwise = auxMenorDivisor n (m+1)

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (a,1,0)
emcd a b = (g,t1,s1-t1*(div a b))
           where (g,s1,t1) = emcd b (mod a b)

tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b c = mod b (mcd a c) == 0

solucionParticular :: Integer -> Integer -> Integer -> Integer
solucionParticular a b c = div (b - c * (div b g) * (ter rec)) a  
                         where rec = emcd (div a g) (div c g)
                               g = mcd a c

pri :: (Integer,Integer,Integer) -> Integer
pri (a,_,_) = a

seg :: (Integer,Integer,Integer) -> Integer
seg (_,b,_) = b

ter :: (Integer,Integer,Integer) -> Integer
ter (_,_,c) = c

solucionGeneral :: Integer -> Integer -> Integer -> (Integer, Integer)
solucionGeneral a b m = (mod ((div a theMCD) * (solucionParticular a b m) * (div b theMCD)) modMCD, modMCD)
                       where theMCD = mcd a m
                             modMCD = div m theMCD