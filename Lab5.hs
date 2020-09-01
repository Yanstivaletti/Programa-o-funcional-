npares :: [Int] -> Int
npares [] = 0
npares (h:t) = if(even h) then npares t + 1
 else npares t 

dobraPos :: [Int] -> [Int]
dobraPos [] = []
dobraPos (h:t) = 2 * h: dobraPos t

produtorio :: [Int] -> Int
produtorio [] = 1
produtorio (h:t) = h * produtorio t

enesimo :: Int -> [Int] -> Int
enesimo n [] = 0
enesimo 1 (h:t) = h
enesimo n (h:t) = enesimo (n-1) t

fatores_aux :: Int -> Int -> [Int]
fatores_aux x 0 = []
fatores_aux x n = if(mod x n == 0) then n:fatores_aux x (n-1) else fatores_aux x (n-1)

fatores :: Int -> [Int]
fatores n = fatores_aux n n

comprime :: [[Int]] -> [Int]
comprime [] = []
comprime (h:t) = h ++ comprime t

tamanho :: Eq a => [a]->Int
tamanho [] = 0
tamanho (h:t) = tamanho t + 1

pertence :: Eq a =>a -> [a] -> Bool
pertence a [] = False
pertence n (h:t)
 | (n == h) = True
 |otherwise = pertence n t

maior :: [Int] -> Int
maior [a] = a
maior (h1:h2:t) = if(h1 > h2) then maior (h1:t)
 else maior(h2:t)

uniaoRec :: [Int] -> [Int] -> [Int]
uniaoRec [] [] = []
uniaoRec xs [] = xs
uniaoRec [] xs = xs
uniaoRec (h1:t1) xs = if(elem h1 xs) then uniaoRec t1 xs
 else h1 : uniaoRec t1 xs

uniaoNRec :: [Int] -> [Int] -> [Int]
uniaoNRec x1 x2 = x1 ++ [n | n <- x2, not(elem n x1)]

union :: [Int]->[Int]->[Int]
union x [] = []
union x (y:z) = if elem y x == True then union x z
 else y:union x z 

uniaoRec2 :: [Int]->[Int]->[Int]
uniaoRec2 x y = x ++ (union x y)