cuadradoTriple x y z = x * x + y * y + z * z

doble x = x + x

suma x y = x + y

normaVectorial :: (Float,Float) -> Float
normaVectorial p = sqrt((tupleFst p)**2 + (tupleSnd p)**2)

funcionConstante8 x = 8

respuestaATodo = 42

tutorialGuarda n | n == 0 = 1
                 | n /= 0 = 0

signo n | n > 0 = 1
        | n == 0 = 0
        | otherwise = (-1)

absoluto n = n * (signo n)

maximo x y | x > y = x
           | otherwise = y

maximo3 x y z = maximo (maximo x y) z

tutorialTipado :: Bool -> Bool
tutorialTipado x = not x

tutorialTipado2 :: Bool -> Float
tutorialTipado2 x = pi

tutorialTipadoFuncion3 :: Integer -> Integer -> Bool -> Bool
tutorialTipadoFuncion3 x y b = b || (x > y)

esPar :: Integer -> Bool
esPar x = (mod x 2) == 0

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = (mod x y) == 0

identidad :: a -> a
identidad x = x

tupleFst :: (a,a) -> a
tupleFst (x,y) = x

tupleSnd :: (a,a) -> a
tupleSnd (x,y) = y

crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)

invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a)

distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (a,b) (c,d) = sqrt ((c-a)^2+(b-d)^2)

f1 :: Float -> (Float,Float,Float)
f1 x = (2 * x,x^2,x-7)

f2 :: Integer -> Integer
f2 n | (mod n 2)==0 = div n 2
     | otherwise = n+1

f :: Integer -> Integer
f n | (mod n 6)==0 = (div (n^2) 2)
    | otherwise = (3 * n) + 1

g :: (Integer,Integer) -> Integer
g (n,m) = n * (m+1)

h :: (Integer,Integer) -> Integer
h (a,b) = f (g (a,b))

unidades :: Integer -> Integer
unidades a = mod a 10

sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 a b c = (unidades a) + (unidades b) + (unidades c)

todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares a b c = not(esPar a) && not(esPar b) && not(esPar c)

alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar a b c = not(esPar a) || not(esPar b) || not(esPar c)

alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares a b c | not(esPar a) && not(esPar b) = True
                        | not(esPar b) && not(esPar c) = True
                        | not(esPar a) && not(esPar c) = True
                        | otherwise = False

alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares a b c | (esPar a) && (esPar b) = True
                      | (esPar b) && (esPar c) = True
                      | (esPar a) && (esPar c) = True
                      | otherwise = False

r1 :: Integer -> Integer -> Bool
r1 a b = mod (a+b) 2 == 0

r2 :: Integer -> Integer -> Bool
r2 a b = (mod (2*a+3*b) 5) == 0

r3 :: Integer -> Integer -> Bool
r3 a b = (mod a 10) /= (mod b 10) && (mod a 10) /= (mod (a*b) 10) && (mod b 10) /= (mod (a*b) 10)

r4 :: Float -> Float -> Bool
r4 a b | (a >= 3) && (b >= 3) = True
       | (a < 3) && (b < 3) = True
       | otherwise = False

r5 :: Float -> Float -> Bool
r5 a b | (a < 3) && (b < 3) = True
       | (a >= 3 && a < 7) && (b >= 3 && b < 7) = True  
       | (a >= 7) && (b >= 7) = True
       | otherwise = False

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n-1)

sc :: Integer -> Integer
sc 0 = 0
sc n = sc (n-1) + n^2

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib a = fib(a-1) + fib(a-2) 

ej16y20a :: Integer -> Integer
ej16y20a 1 = 2
ej16y20a n = (2 * n * ej16y20a (n-1)) + (2^(n+1)*factorial n)

ej16y20b :: Integer -> Integer
ej16y20b 1 = (-3)
ej16y20b 2 = 2
ej16y20b n = n * ej16y20b (n-1) + 2 * (n+1) * ej16y20b (n-1)

ej16y20c :: Integer -> Integer
ej16y20c 1 = (-3)
ej16y20c 2 = 6
ej16y20c n | (mod n 2) == 1 = (-1)*ej16y20c (n-1) - 3
           | otherwise = ej16y20c (n-1) + ej16y20c (n-2) + 9

ej5a :: Integer -> Integer
ej5a 0 = 1
ej5a n = (2^n)*ej5a (n-1)

ej5b :: Integer -> Float -> Float
ej5b 0 q = 0
ej5b n q = (q^n) + ej5b (n-1) q

ej5c :: Integer -> Float -> Float
ej5c 0 q = 0
ej5c n q = ej5c (n-1) q + q^(n-1) + q^n

ej5d :: Integer -> Float -> Float
ej5d 0 q = 1/2
ej5d n q = (ej5d (n-1) q) + (q^(2*n))/2 + (q^(2*n-1))/2 - (q^n)/2

soyPar :: Integer -> Bool
soyPar 0 = True
soyPar 1 = False
soyPar n = soyPar (n-2)

mult3 :: Integer -> Bool
mult3 0 = True
mult3 1 = False
mult3 2 = False
mult3 n = mult3 (n-3)

sumaImpares :: Integer -> Integer
sumaImpares 1 = 1
sumaImpares n = (2*n)-1 + sumaImpares (n-1)

doblefact :: Integer -> Integer
doblefact 2 = 2
doblefact n = n * doblefact (n-2)

notermina :: Integer -> Integer
notermina n | n < 0 = notermina (n-1)
            | otherwise = n

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = eAprox (n-1) + 1/fromInteger (factorial n)

e :: Float
e = eAprox 100

parteEntera :: Float -> Integer
parteEntera x | x<0 = (-1)*parteEntera (x*(-1))
              | x<1 = 0
              | otherwise = parteEntera (x-1) + 1

algDivision :: Integer -> Integer -> (Integer,Integer)
algDivision a d | a<d = (0,a)
                | otherwise = (1 + fst qr,snd qr)
                where qr = algDivision (a-d) d

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n 1 = 1
sumaDivisoresHasta n m | mod n m == 0 = m + (sumaDivisoresHasta n (m-1))
                       | otherwise = (sumaDivisoresHasta n (m-1))

menorDivisor :: Integer -> Integer
menorDivisor n = auxMenorDivisor n 2

auxMenorDivisor :: Integer -> Integer -> Integer
auxMenorDivisor n m | mod n m == 0 = m
                    | otherwise = auxMenorDivisor n (m+1)

esPrimo :: Integer -> Bool
esPrimo n = (menorDivisor n) == n

sumatoriaDoble :: Integer -> Integer -> Integer
sumatoriaDoble n m = auxSumatoriaDoble n m m

auxSumatoriaDoble :: Integer -> Integer -> Integer -> Integer
auxSumatoriaDoble 1 1 a = 1 
auxSumatoriaDoble n 1 a = n + auxSumatoriaDoble (n-1) a a
auxSumatoriaDoble n m a = (n^m) + auxSumatoriaDoble n (m-1) a 

sumaPotencias :: Float -> Integer -> Integer -> Float
sumaPotencias q n m = auxSumaPotencias q n m m

auxSumaPotencias :: Float -> Integer -> Integer -> Integer -> Float
auxSumaPotencias q 1 1 _ = q^2
auxSumaPotencias q n 1 a = q^(n+1) + auxSumaPotencias q (n-1) a a
auxSumaPotencias q n m a = q^(n+m) + auxSumaPotencias q n (m-1) a

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n m = auxSumaRacionales n m m

auxSumaRacionales :: Integer -> Integer -> Integer -> Float
auxSumaRacionales 1 1 _ = fromInteger 1
auxSumaRacionales n 1 a = fromInteger n + auxSumaRacionales (n-1) a a
auxSumaRacionales n m a = (fromInteger n)/(fromInteger m) + auxSumaRacionales n (m-1) a

yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _ = False

oLogico :: Bool -> Bool -> Bool
oLogico False False = False
oLogico _ _ = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana (n-1)

algunoEsCero :: (Integer,Integer,Integer) -> Bool
algunoEsCero (0,_,_) = True
algunoEsCero (_,0,_) = True
algunoEsCero (_,_,0) = True
algunoEsCero _ = False

productoInterno :: (Float,Float) -> (Float,Float) -> Float
productoInterno (x1,y1) (x2,y2) = x1*x2 + y1*y2

esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos n = auxEsSumaDeDosPrimos 1 (n-1)

auxEsSumaDeDosPrimos :: Integer -> Integer -> Bool
auxEsSumaDeDosPrimos n 1 = False
auxEsSumaDeDosPrimos n m = (esPrimo n && esPrimo m) || auxEsSumaDeDosPrimos (n+1) (m-1)

conjeturaGoldbach :: Integer -> Bool
conjeturaGoldbach 4 = True
conjeturaGoldbach n = esSumaDeDosPrimos n && conjeturaGoldbach (n-2)

sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

digitosIguales :: Integer -> Bool
digitosIguales n | n < 10 = True
                 | otherwise = mod n 10 == mod (div n 10) 10 && digitosIguales (div n 10)

collatz :: Integer -> Integer
collatz 1 = 1
collatz n | mod n 2 == 0 = 1 + collatz (div n 2)
          | otherwise = 1 + collatz ((3*n)+1)

bestCollatz :: Integer -> Integer
bestCollatz n = auxBestCollatz n n

auxBestCollatz :: Integer -> Integer -> Integer
auxBestCollatz 1 m = m
auxBestCollatz n m | collatz n > collatz m = auxBestCollatz (n-1) n
                   | otherwise = auxBestCollatz (n-1) m

listar :: a -> a -> a -> [a]
listar a b c = [a,b,c]

pertenece :: Integer -> [Integer] -> Bool
pertenece v [] = False
pertenece v (x:xs) = v == x || pertenece v xs 

productoriaSinPM :: [Integer] -> Integer
productoriaSinPM x | x == [] = 1
                   | otherwise = head x * productoriaSinPM (tail x)

productoriaConPM :: [Integer] -> Integer 
productoriaConPM (x:[]) = x
productoriaConPM (x:xs) = x * productoriaConPM xs

sumarNSinPM :: Integer -> [Integer] -> [Integer]
sumarNSinPM n xs | (tail xs) == [] = (head xs) + n : []
                 | otherwise = (head xs) + n : sumarNSinPM n (tail xs)

sumarNConPM :: Integer -> [Integer] -> [Integer]
sumarNConPM n (x:[]) = x+n : []
sumarNConPM n (x:xs) = x+n : sumarNConPM n xs

ultimoLista :: [Integer] -> Integer
ultimoLista (x:[]) = x
ultimoLista (_:xs) = ultimoLista xs

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo x = sumarNConPM (ultimoLista x) x 


sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarNConPM x (x:xs)

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x: pares xs
             | otherwise = pares xs

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x: multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n (x:xs) | x == n = xs
                | otherwise = x:(quitar n xs)

hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs

eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = eliminarRepetidos xs
                         | otherwise = x : eliminarRepetidos xs

maximo2 :: [Integer] -> Integer
maximo2 (x:[]) = x
maximo2 (x:xs) | x > maximo2 xs = x
               | otherwise = maximo2 xs 

ordenar :: [Integer] -> [Integer]
ordenar (x:[]) = [x]
ordenar (x:xs) | x <= head ts = x:ts
               | otherwise = ordenar (xs++[x])
               where ts = ordenar xs