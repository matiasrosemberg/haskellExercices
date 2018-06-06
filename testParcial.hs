menorLex :: (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> Bool
menorLex (a,b,c) (d,e,f) | a < d = True
                         | a == d && b < e = True
                         | a == d && b == e && c < f = True
                         | otherwise = False

nFibonacci :: Integer -> Integer
nFibonacci 0 = 1
nFibonacci 1 = 1
nFibonacci n = nFibonacci (n-1) + nFibonacci (n-2)

sumaFibonacci :: Integer -> Integer
sumaFibonacci 1 = 2
sumaFibonacci n = nFibonacci n + sumaFibonacci (n-1)

sumDivisoresDe :: Integer -> Integer -> Integer
sumDivisoresDe n 1 = 1
sumDivisoresDe n x | mod n x == 0 = x + sumDivisoresDe n (x-1)
                   | otherwise = sumDivisoresDe n (x-1)

esDefectivo :: Integer -> Bool
esDefectivo n = (sumDivisoresDe n (n-1)) < n

maximaDistancia :: [Integer] -> Integer
maximaDistancia (x:y:[]) = abs (x-y)
maximaDistancia (x:y:xs) | maximaDistancia (y:xs) >= abs (x-y) = maximaDistancia (y:xs)
                         | otherwise = abs (x-y)

sonAmigos :: Integer -> Integer -> Bool
sonAmigos a b = (sumDivisoresDe a (a-1)) == b && (sumDivisoresDe b (b-1))== a