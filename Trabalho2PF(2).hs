
--Listas para usar nos algorítmos
l1 = [1..1000] 
l2 = [1000,999..1]
l3 = l1++[0]
l4 = [0]++l2
l5 = l1++[0]++l2
l6 = l2++[0]++l1
l7 = l2++[0]++l2
x1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]
lst = [l1,l2,l3,l4,l5,l6,l7,x1,x2,x3,x4,x5,x6,x7]
lst1 = [l1,l2,l3,l4,l5,l6,l7]
lst2 = [x1,x2,x3,x4,x5,x6,x7]
bol0 = map bubbleSortContador lst2
bol1 = map bubbleSort1Contador lst2
bol2 = map bubbleSort3Contador lst2
b0 = map snd (map bubbleSortContador lst2)
b1 = map snd (map bubbleSort1Contador lst2)
b2 = map snd (map bubbleSort3Contador lst2)
q5 = map snd (map quickSort3Contador lst)
q6 = map snd (map quickSort2Cont lst)

--Exercício 1

bubbleSort [] = []
bubbleSort lista = bubbleOrd lista (length lista)

bubbleOrd lista 0 = lista
bubbleOrd lista n = bubbleOrd (troca lista) (n-1)

troca [x] = [x]
troca (x:y:z)
 |x > y = y:troca(x:z)
 |otherwise = x:troca(y:z)
 
 
--BubbleSort Com Contador
bubbleSortContador :: [Integer] -> ([Integer], Int)
bubbleSortContador [] = ([], 0)
bubbleSortContador xs = bubbleOrdContador xs (length xs)

bubbleOrdContador :: [Integer]->Int->([Integer], Int)
bubbleOrdContador xs 0 = (xs, 0)
bubbleOrdContador xs n = (d, a + b)
 where
 (aux, a) = trocaContador xs
 (d, b) = bubbleOrdContador aux (n-1)

trocaContador :: [Integer] -> ([Integer], Int)
trocaContador [x] = ([x],0)
trocaContador (x:y:z) 
 |x > y = (y:d, a + 1)
 |otherwise = (x:n, b + 1)
 
 where
 (d, a) = trocaContador(x:z)
 (n, b) = trocaContador(y:z)

--Variação1
bubbleSort1 :: [Integer] -> [Integer]
bubbleSort1 [] = []
bubbleSort1 xs = bubbleOrd1 xs 1

bubbleOrd1 :: [Integer]-> Integer -> [Integer]
bubbleOrd1 xs 0 = xs
bubbleOrd1 xs n = a
 where
 (aux, num) = troca1 xs
 a = bubbleOrd1 aux num


troca1 :: [Integer] -> ([Integer],Integer)
troca1 [x] = ([x],0)
troca1 (x:y:t)
 |x > y = (x1, num1 + 1)
 |otherwise = (x2, num2)
 where
 (aux1, num1) = troca1(x:t)
 x1 = y:aux1
 (aux2, num2) = troca1(y:t)
 x2 = x:aux2


--Variação2
bubbleSort2 :: [Integer] -> [Integer]
bubbleSort2 [] = []
bubbleSort2 xs = bubbleOrd2 xs (length xs)

bubbleOrd2 :: [Integer]-> Int -> [Integer]
bubbleOrd2 xs 0 = xs
bubbleOrd2 xs n = bubbleOrd2 (troca2 xs n) (n-1)


troca2 :: [Integer] -> Int -> [Integer]
troca2 xs 1 = xs
troca2 (x:y:t) n 
 |x > y = y:troca2(x:t) (n-1)
 |otherwise = x:troca2(y:t) (n-1)


--Variação3
bubbleSort3 :: [Integer] -> [Integer]
bubbleSort3 [] = []
bubbleSort3 xs = bubbleOrd3 xs 1 (length xs)


bubbleOrd3 :: [Integer]-> Int -> Int -> [Integer]
bubbleOrd3 xs 0 _ = xs
bubbleOrd3 xs _ 0 = xs
bubbleOrd3 xs contador tam = j
 where
 (aux, num) = troca3 xs tam
 j = bubbleOrd3 aux num (tam-1)
 

troca3 :: [Integer] -> Int -> ([Integer],Int)
troca3 xs 1 = (xs,0)
troca3 (x:y:t) n
 |x > y = (x1, n1 + 1) 
 |otherwise = (x2, n2)
 where
 (aux1, n1) = troca3 (x:t) (n-1)
 x1 = y:aux1
 (aux2, n2) = troca3 (y:t) (n-1)
 x2 = x:aux2


--Variação1comContador
bubbleSort1Contador :: [Integer] -> ([Integer], Int)
bubbleSort1Contador [] = ([], 0)
bubbleSort1Contador xs = bubbleOrd1Contador xs 1

bubbleOrd1Contador :: [Integer] -> Integer -> ([Integer], Int)
bubbleOrd1Contador xs 0 = (xs, 0)
bubbleOrd1Contador xs n = (d, a + b)
 where
 (aux, num, b) = troca1Contador xs
 (d, a) = bubbleOrd1Contador aux num 


troca1Contador :: [Integer] -> ([Integer],Integer, Int)
troca1Contador [x] = ([x], 0, 0)
troca1Contador (x:y:z)
 |x > y = (x1, n1 + 1, a + 1)
 |otherwise = (x2, n2, a2 + 1)
 where
 (aux1, n1, a) = troca1Contador(x:z)
 x1 = y:aux1
 (aux2, n2, a2) = troca1Contador(y:z)
 x2 = x:aux2


--Variação2comContador
bubbleSort2Contador :: [Integer] -> ([Integer], Int)
bubbleSort2Contador [] = ([], 0)
bubbleSort2Contador xs = bubbleOrd2Contador xs (length xs)

bubbleOrd2Contador :: [Integer]-> Int -> ([Integer], Int)
bubbleOrd2Contador xs 0 = (xs, 0)
bubbleOrd2Contador xs n = (d, a + b)
 where
 (aux, b) = troca2Contador xs n 
 (d, a) = bubbleOrd2Contador aux (n-1)


troca2Contador :: [Integer] -> Int -> ([Integer], Int)
troca2Contador l 1 = (l, 0)
troca2Contador (x:y:z) t 
 |x > y = (y:m, a + 1)
 |otherwise = (x:n, b + 1)
 
 where
 (m, a) = troca2Contador(x:z) (t - 1)
 (n, b) = troca2Contador(y:z) (t - 1)


--Variação3comContador
bubbleSort3Contador :: [Integer] -> ([Integer], Int)
bubbleSort3Contador [] = ([], 0)
bubbleSort3Contador lista = bubbleOrd3contador lista 1 (length lista)


bubbleOrd3contador :: [Integer]-> Int -> Int -> ([Integer], Int)
bubbleOrd3contador lista 0 _ = (lista, 0)
bubbleOrd3contador lista _ 0 = (lista, 0)
bubbleOrd3contador lista cont tamanho = (n, a + b)
 where
 (aux, num, b) = troca3Contador lista tamanho
 (n, a) = bubbleOrd3contador aux num (tamanho-1) 
 

troca3Contador :: [Integer] -> Int -> ([Integer],Int, Int)
troca3Contador xs 1 = (xs, 0, 0)
troca3Contador (x:y:z) n
 |x > y = (x1, n1 + 1, a+1) 
 |otherwise = (x2, n2, b+1)
 where
 (aux1, n1, a) = troca3Contador (x:z) (n-1)
 x1 = y:aux1
 (aux2, n2, b) = troca3Contador (y:z) (n-1)
 x2 = x:aux2
{-

       Var 1        Var 2 		Var 3

l1 =	999			499500		999
l2 =   999000       499500      499500
l3 =   1001000      500500      500500
l4 =   1000000      500500      500500
l5 =   3998000      2001000     2000999
l6 =   2002000      2001000     1501500
l7 =   3998000      2001000     2000999



-}

{-
Dentre as comparaçoes feitas, podemos considerar que o bubbleSort3 
é mais eficiente pela velocidade que é uma das mais rapidas e também
pelo contador que mostra o menor número de comparaçoes possível
-}


--Exercício 2

--Sem variação


--Variação 1
selectionSort1 :: [Integer] -> [Integer]
selectionSort1 [] = []
selectionSort1 xs = x:selectionSort1 (remove1 x xs)
 where x = minimo1 xs

remove1 ::  Integer -> [Integer] -> [Integer]
remove1 a [] = []
remove1 a (x:xs)
 | a == x = xs
 | otherwise = x:(remove1 a xs)

minimo1 :: [Integer] -> Integer
minimo1 [] = undefined
minimo1 [x] = x
minimo1 (x:xs)
 | x <= (minimo1 xs) = x
 | otherwise = minimo1 xs

--Variação 2
selectionSort2 :: [Integer] -> [Integer]
selectionSort2 [] = []
selectionSort2 ys = x:selectionSort2 xs
 where (x,xs) = minimo2 ys

minimo2 :: [Integer] -> (Integer,[Integer])
minimo2 [] = undefined
minimo2 [x] = (x,[])
minimo2 (x:xs)
 | x <= y = (x,xs)
 | otherwise = (y,x:z)
 where (y,z) = minimo2 xs

--Variação 2 com contador
selectionSort2Contador :: [Integer] -> ([Integer],Integer)
selectionSort2Contador [] = ([],0)
selectionSort2Contador ys = (x:y,n+a)
 where 
    (x,xs,a) = minimo2Contador (ys,n)
    (y,n) = selectionSort2Contador xs

minimo2Contador :: ([Integer],Integer) -> (Integer,[Integer],Integer)
minimo2Contador ([],n) = (undefined,[],n)
minimo2Contador ([x],n) = (x,[],n)
minimo2Contador (x:xs,n)
 | x <= y = (x,xs,a+1)
 | otherwise = (y,x:z,a+1)
 where (y,z,a) = minimo2Contador (xs,n)
{-
Dentre as Variaçoes, podemos eleger a Variação dois como a mais eficiente, usando
a lista l7, nota-se que a unica capaz de carregar o resultado em um tempo eficiente
é a Variaçao 2.

-}

--Exercício 3

--insertionSort com Contador
insertionSortCont :: [Integer] -> ([Integer],Integer)
insertionSortCont [] = ([],0)
insertionSortCont (x: xs) = (l,c + c1)
 where
  (l1,c) = insertionSortCont xs
  (l,c1) = insertCont x (l1,0)

insertCont :: Integer -> ([Integer],Integer) -> ([Integer],Integer)
insertCont x ([],c) = ([x],c)
insertCont x ((y:ys),c)
 | x <= y = (x : y : ys, c+1)
 | otherwise = (y :l, c1+1)
 where
  (l,c1) = insertCont x (ys,c)
--l7 = 1.500.501 comparaçoes

-- variação 1
insertionSort1 :: [Integer] -> [Integer]
insertionSort1 = foldr insert []
 
insert :: Integer -> [Integer] -> [Integer]
insert x [] = [x]
insert x (y:ys)
 | x < y = x : y : ys
 | otherwise = y : insert x ys

-- variaçao1 com contador

insertionSort1Contador :: [Integer] -> ([Integer],Integer)
insertionSort1Contador xs = foldr insert1 ([],0) xs

insert1 :: Integer -> ([Integer],Integer) -> ([Integer],Integer)
insert1 x ([],c) = ([x],c)
insert1 x ((y:ys),c)
 | x <= y = (x : y : ys, c+1)
 | otherwise = (y :l, c1+1)
 where
  (l,c1) = insert1 x (ys,c)
  
  
  {-
        InserctionNormal   Var 1
l1 =     999               999
l2 =     499500           499500
l3 =     1999             1999
l4 =     499501           499501
l5 =     1001001          1001001
l6 =     1002000          1002000
l7 =     1500501          1500501



O uso de um contador no codigo não parece afetar o numero de comparaçoes.
Sendo ambos bem rapidos e eficientes
-}

--Exercicio 4
--Variaçao 1

quickSort1 :: (Ord a) => [a] -> [a]
quickSort1 [] = []
quickSort1 (x:xs) = (quickSort1 menores) ++ [x] ++ (quickSort1 maiores)
 where
   menores = filter (< x) xs
   maiores = filter (>= x) xs

--Variaçao 2
quickSort2 :: [Integer] -> [Integer]
quickSort2 [] = []
quickSort2 (x:xs) = (quickSort2 xs1) ++ [x] ++ (quickSort2 xs2)
 where 
    (xs1,xs2) = divide x xs


divide :: Integer -> [Integer] -> ([Integer],[Integer])
divide x xs = ((filter (< x) xs),filter (>= x) xs)

--Variaçao 2 Com Contador
quickSort2Cont :: [Integer] -> ([Integer],Integer)
quickSort2Cont [] = ([],0)
quickSort2Cont (x:xs) = (xs1 ++ [x] ++ xs2, c + c1 + c2)
 where 
    (xs3,xs4,c) = divideContador x (xs,0)
    (xs1,c1) = quickSort2Cont xs3
    (xs2,c2) = quickSort2Cont xs4


divideContador :: Integer -> ([Integer],Integer) -> ([Integer],[Integer],Integer)
divideContador x ([],c) = ([],[],0)
divideContador x ((y:ys),c)
 | y < x = (y:x1,x2,c+1)
 |otherwise = (x1,y:x2,c+1)
 where
  (x1,x2,c) = divideContador x (ys,c)


--Variaçao 3
quickSort3 :: [Integer] -> [Integer]
quickSort3 [] = []
quickSort3 (x1:x2:x3:xs) = (quickSort3 xs1) ++ [r] ++ (quickSort3 xs2)
 where
   (r1,r,r2) = mediana (x1,x2,x3)
   (xs1,xs2) = divide r (r2:r1:xs)
quickSort3 (x1:xs) = quickSort3 xs1 ++[x1] ++ quickSort3 xs2
  where 
   (xs1,xs2) = divide x1 xs

mediana :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
mediana (x,y,z)
 | (x >= y && x <= z) || (x <=y) && (x >= z) = (y,x,z)
 | (y >= x && y <= z) || (y <=x) && (y >= z) = (x,y,z)
 |otherwise = (x,z,y)

--Variaçao 3 Com Contador
quickSort3Contador :: [Integer] -> ([Integer],Integer)
quickSort3Contador [] = ([],0)
quickSort3Contador (x1:x2:x3:xs) = ((xs3) ++ [r] ++ (xs4),c + c1 + c2)
 where
   (r1,r,r2) = mediana (x1,x2,x3)
   (xs3,xs4,c) = divideContador r ((r2:r1:xs),0)
   (xs1,c1) = quickSort3Contador xs3
   (xs2,c2) = quickSort3Contador xs4
quickSort3Contador (x1:xs) = ((xs3 ++[x1] ++ xs4),c + c1 + c2)
  where 
   (xs3,xs4,c) = divideContador x1 (xs,0)
   (xs1,c1) = quickSort3Contador xs3
   (xs2,c2) = quickSort3Contador xs4

{-


		Var 2		Var 3

l1 =	499500		250000
l2 = 	499500		250000
l3 = 	499501		250002
l4 = 	500500		250500
l5 = 	1001000		502000
l6 = 	1001000		502000
l7 = 	1001000		502000



Mesmo as variaçoes tendo tempo de execuçao relativamente parecidos, a 
variaçao 3 apresentou um número menor de comparaçoes, sendo portanto a
mais eficiente dentre elas

-}


--Exercicio 5
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort primeiros) (mergeSort ultimos)
 where 
      primeiros = take (div ( length xs)  2) xs
      ultimos   = drop (div ( length xs)  2) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
 | x < y = x : (merge xs (y:ys))
 | otherwise = y : (merge (x:xs) ys)
 
--Variação com Contador 
 
mergeSortContador :: (Ord a) => [a] -> ([a], Int)
mergeSortContador [] = ([], 0)
mergeSortContador [a] = ([a], 0)
mergeSortContador xs = (ys,c+c1+c2)
 where
  (ys,c) = mergeContador m n
  (m,c1) = mergeSortContador primeiros
  (n,c2) = mergeSortContador ultimos
  primeiros = take ((length xs) `div` 2) xs
  ultimos = drop ((length xs) `div` 2) xs

mergeContador :: (Ord a) => [a] -> [a] -> ([a],Int)
mergeContador xs [] = (xs,0)
mergeContador [] xs = (xs,0)
mergeContador (x:xs) (y:ys)
 | x < y = (x:xs1,c+1)
 | otherwise = (y:xs2,d+1)
  where
   (xs1,c) = mergeContador xs (y:ys)
   (xs2,d) = mergeContador (x:xs) ys
   
   
   
{-   
usando l7 como comparaçao

 Bubble: 4002000 comparaçoes 
 Selection: 2029000 comparaçoes
 Inserction: 1500501 comparaçoes
 Quick: 1001000 comparaçoes
 Merge: 12100 comparaçoes
 
 Evidentemente, MergeSort é o melhor algoritmo dentre todos,
 tanto pelo seu numero de comparaçoes, quanto seu tempo de processamento


 -}



