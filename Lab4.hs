fatorial :: Int->Int
fatorial 0 = 1
fatorial n = n * fatorial(n-1)

fibo :: Int -> Int
fibo 1 = 1
fibo 2 = 1
fibo n = fibo(n-2) + fibo(n-1)

n_tri :: Int -> Int
n_tri 1 = 1
n_tri n = n + n_tri(n-1)

passo :: (Int,Int) -> (Int,Int)
passo (n,m) = (m,n+m)

fibaux :: Int -> (Int, Int) -> Int
fibaux n (x,y) 
 |n == 0 = x
 |otherwise = fibaux (n-1) (passo (x,y)) 

fib1 :: Int -> Int
fib1 x = fibaux x (0,1)

comparafibo :: [Int]
comparafibo  = [fibo n | n <- [0..50]]

comparafib1 :: [Int]
comparafib1  = [fib1 n | n <- [0..50]]




potencia2 :: Int->Int
potencia2 1 = 2
potencia2 n = 2 ^ (n-1) * 2

prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
 | n == m = m
 | n < m = 0
 |otherwise = m * prodIntervalo (m+1) n

fat :: Int -> Int
fat 0 = 1
fat n = prodIntervalo 1 n

resto_div :: Int-> Int -> Int
resto_div dividendo divisor
 |divisor == 1 = 0
 |divisor == dividendo = 0
 |dividendo < divisor = dividendo
 |otherwise = resto_div (dividendo-divisor) divisor
 

div_inteira :: Int -> Int -> Int
div_inteira dividendo divisor
 | divisor == 0 = -1
 | divisor == 1 = dividendo
 | dividendo < divisor = 0
 |otherwise = div_inteira (dividendo-divisor) divisor + 1

--  mdc :: (Int,Int) -> Int
--  mdc (m,0) = m
--  mdc (m,n) = mdc (n, (mod m n))

-- binomialg :: (Int,Int) -> Int
-- binomialg (n,0)  = 1binomialg (n,k)  
--  | k == 0 = 1  
--  | k == n = 1  
--  | otherwise = binomialg (n-1,k) + binomialg (n-1,k-1)