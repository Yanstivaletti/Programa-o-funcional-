--Ex1
a:: Int -> Int
a y = (2 * y) + 1 

b:: Int -> Int -> Int
b x y = x - y

c :: Int -> Int ->Int
c y x = x - y

--d :: Int ->  Int -> Int
d z y = (z/2) - y

--e :: Int ->  Int -> Int -- entrada 6 e 1
e z y = ((/) z 2) - y   -- saida 2

f :: Int -> Int -> Int
f x y = c (-x) y --tomamos lambida y como a funçao c 
{-
Ex 2
*Main> (\x -> x + 3) 5
8
*Main> (\x -> \y -> x * y + 5) 3 4
17
*Main> map (\x -> x^3) [2,4,6]
[8,64,216]
*Main> (\(x,y) -> x * y^2) (3,4)
48
*Main> (\(x,y,_) -> x * y^2) (3,4,2)
48
*Main> map (\(x,y,z) -> x + y + z) [(3,4,2), (1,1,2), (0,0,4)]
[9,4,4]
*Main> filter (\(x,y) -> x `mod` y == 0) [(4,2), (3,5), (6,3)]
[(4,2),(6,3)]
*Main> (\xs -> zip xs [1,2,3]) [4,5,6]
[(4,1),(5,2),(6,3)]
*Main> map (\xs -> zip xs [1..]) [[4,6], [5,7]]
[[(4,1),(6,2)],[(5,1),(7,2)]]
*Main> foldr1 (+) [1,2,3]
6
*Main> foldr1 (\x -> \y -> x + y + 7) [1,2,3,4,5]
43
*Main> foldr (\x -> \y -> y + 5) 100 [4,77,1]
115
-}
--Ex3
--(λx λy. y)((λz. z)(λz. z))(λw. w) 5
--(λx λy. y)(λz. z)(λw. w) 5
x1 :: Int -> Int
x1 a = y1 a
y1 :: Int -> Int
y1 a = a
z1 :: Int -> Int
z1 a = a
w1 :: Int -> Int 
w1 a = a

((λf. (λx. f(f x))) (λy. (y * y))) 3

twice::(a->a)->a->a
twice f x = f (f x)

y2 :: Int -> Int
y2 a = a * a 

--((λf. (λx. f(f x)))(λy.(+ y y))) 5
y3 :: Int -> Int
y3 a = a + a

--((λx. (λy. + x y) 5) ((λy. - y 3) 7))

x4 :: Int -> Int -> Int
x4 x = (y4 x) + 5

--y4 :: Int -> Int
--y4 y =  y - 3 

(((λf. (λx. f(f(f x)))) (λy. (y * y))) 2)
square :: Int -> Int 
square y = y * y

--(λx. λy. + x ((λx. - x 3) y)) 5 6
x5_1 :: Int -> Int -> Int 
x5_1 a b = y5 a + x5_2 (-b) 3

x5_2 :: Int -> Int -> Int
x5_2 x y = x - y 

{-
Prelude> (\x -> \y -> y)( (\z -> z) (\z -> z))(\w -> w) 5 
5
Prelude> (\f -> (\x -> f(f x)))(\y -> (y * y)) 3
81
Prelude> (\f -> (\x -> f(f x)))(\y -> (y + y)) 5
20
Prelude> ((\x -> (\y -> x + y)5)((\y -> y - 3) 7))
9
Prelude> (((\f -> (\x -> f(f(f x))))(\y -> (y * y)))2)
256
Prelude> (\x -> \y -> x + ((\x -> x - 3)y)) 5 6
8
-}