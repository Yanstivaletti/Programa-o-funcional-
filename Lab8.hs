--Ex 3
dobrar :: [Int] -> [Int]
dobrar xs = map(\x -> x * 2) xs
--Ex 4
paridade :: [Int] -> [Bool]
paridade xs = map(\x -> par x) xs

par :: Int -> Bool
par x = if even x then True
 else False
--Ex 5
prefixos :: [String] -> [String]
prefixos xs = map(take 3) xs
--Ex 6
saudacao :: [String] -> [String]
saudacao xs = map(\x -> "Oi "++ x)xs
--Ex 7
pares :: [Int] -> [Int]
pares xs = filter(\x -> par x) xs
--Ex9
solucoes:: [Int] -> [Int]
solucoes xs = filter(\x -> 2*x < x+5) xs
--Ex10
produto :: [Int] -> Int
produto xs = foldr1 (*) xs
--Ex11
maior :: [Int] -> Int
maior xs = foldr1 max xs
--Ex12
concatTodos :: [String] -> String
concatTodos xs = foldr1 (++) xs
--Ex13
menor :: Int ->  Int ->  Int
menor a b = if a < b then a
 else b
menor_min10 :: [Int] -> Int
menor_min10 x = foldr menor 10 x
--Ex14
junta_silabas_plural :: [String] -> String
junta_silabas_plural x = foldr (++) "s" x