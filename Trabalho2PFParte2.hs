--Exercicio 6

data Bilhete = Trem Cidade Cidade Classe | Onibus Cidade Cidade | Aviao Cidade Cidade Classe
 deriving (Show)
type Classe = String
type Cidade = String

type Viagem = [Bilhete]
p2 :: Bilhete 
p2 = Trem "1" "2" "Primeira"

p1 :: Viagem 
p1 = [Trem "Franca" "Uberlandia" "Primeira", Onibus "Caracas" "Araguari"]

viagemIda :: Bilhete -> String
viagemIda (Onibus x _) = x
viagemIda (Trem x _ _) = x
viagemIda (Aviao x _ _) = x

viagemVolta :: Bilhete -> String
viagemVolta (Onibus _ x) = x
viagemVolta (Trem _ x _) = x
viagemVolta (Aviao _ x _) = x


viagemValida :: Viagem -> Bool
viagemValida [] = True
viagemValida [x] = True
viagemValida (x:y:xs) = if (viagemVolta x == viagemIda y) then viagemValida (y:xs)
 else False 

-----------------
--Exercicio 7

data Exp a = Val a | Add (Exp a) (Exp a) | Sub (Exp a) (Exp a) | Mul (Exp a) (Exp a) | Div (Exp a) (Exp a) | Neg (Exp a)
 deriving (Show,Eq)

avalia :: (Floating a) => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = (avalia exp1) / (avalia exp2)


--f1 = Div (Mul (Add (Val 3) (Val 12)) (Sub (Val 15) (Val 5))) (Div (Val 10) (Val 2))

--f2 = Mul (Sub (Add (Add (Val 6) (Val 8)) (Add (Val 5) (Val 1))) (Mul (Val 2) (Val 20))) (Add (Val 2) (Div (Val 6) (Val 2)))  



------------------
--Ex 8
data LL = Latitude Int Int Int | Longitude Int Int Int
 deriving (Eq,Show)

type PosicaoLocal = (String, LL, LL)
type Cidades = [PosicaoLocal]

c1,c2::PosicaoLocal
c1=("Brasilia", Latitude (-15) 46 47, Longitude 47 55 47)
c2=("Uberlandia", Latitude (-18) 55 07, Longitude 48 16 38)
lcidades::Cidades
lcidades = [("Rio Branco", Latitude 09 58 29, Longitude 67 48 36),("Brasilia", Latitude (-15) 46 47, Longitude 47 55 47),("Torres", Latitude (-29) 20 07, Longitude 49 43 37),("Joao Pessoa", Latitude (-07) 06 54, Longitude 34 51 47),("Uberlandia", Latitude (-18) 55 07, Longitude 48 16 38)]


extraiX:: LL -> Int
extraiX (Latitude x _ _) = x
extraiX (Longitude x _ _) = x 

extraiY:: LL -> Int
extraiY (Latitude _ y _) = y
extraiY (Longitude _ y _) = y 

extraiZ:: LL -> Int
extraiZ (Latitude _ _ z) = z
extraiZ (Longitude _ _ z) = z 

extraiLat :: PosicaoLocal -> (String,Int,Int,Int)
extraiLat (cidade,lat,long) = (cidade,extraiX lat, extraiY lat, extraiZ lat)

extraiLong :: PosicaoLocal -> (String,Int,Int,Int)
extraiLong (cidade,lat,long) = (cidade,extraiX long, extraiY long, extraiZ long)

norteDe :: PosicaoLocal -> PosicaoLocal -> Bool
norteDe (_,lat1,_) (_,lat2,_) = if (extraiX lat1 > extraiX lat2) then True
 else False  

sulDe :: PosicaoLocal -> PosicaoLocal -> Bool
sulDe (_,lat1,_) (_,lat2,_) = if (extraiX lat1 < extraiX lat2) then True
 else False  

oesteDe :: PosicaoLocal -> PosicaoLocal -> Bool
oesteDe (_,_,long1) (_,_,long2) = if (extraiX long1 > extraiX long2) then True
 else False  

lesteDe :: PosicaoLocal -> PosicaoLocal -> Bool
lesteDe (_,_,long1) (_,_,long2) = if (extraiX long1 < extraiX long2) then True
 else False  

abaixoDoEq :: Cidades -> Cidades
abaixoDoEq [] = []
abaixoDoEq ((x,y,z):xs) 
 |(extraiX y) < 0 = (x,y,z) : abaixoDoEq xs
 |otherwise =  abaixoDoEq xs

cidadesEntre :: Cidades -> Int -> Int -> Cidades
cidadesEntre [] a b = []
cidadesEntre ((x,y,z):xs) a b
 |((extraiX z) >= a) && ((extraiX z) <= b) = (x,y,z) : cidadesEntre xs a b
 |otherwise =  cidadesEntre xs a b

--Exercicio 9
data ArvoreBinInt = Nulo1 | No1 Int ArvoreBinInt ArvoreBinInt
 deriving(Show,Eq)

internos:: ArvoreBinInt ->  [Int]
internos Nulo1 = []
internos (No1 x esq dir) = [x] ++ internos esq ++ internos dir 

somanos:: ArvoreBinInt -> Int
somanos Nulo1 = 0
somanos (No1 x esq dir) = x + somanos esq + somanos dir

pertence :: ArvoreBinInt -> Int -> Bool
pertence Nulo1 a = False
pertence (No1 x esq dir) a 
 | x == a = True
 | otherwise = ( pertence esq a || pertence dir a)  


--Ex 10

data ArvBinEA a = Vazia | Folha a | NoEA (Char, ArvBinEA a, ArvBinEA a)
 deriving (Show,Eq)

ea::ArvBinEA Float
ea = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)


resolveABEA:: ArvBinEA Float -> Float
resolveABEA (Vazia) = 0
resolveABEA (Folha x) = x
resolveABEA (NoEA (x,esq,dir))
 | x == '+' = resolveABEA esq + resolveABEA dir
 | x == '-' = resolveABEA esq - resolveABEA dir
 | x == '*' = resolveABEA esq * resolveABEA dir
 | otherwise = resolveABEA esq / resolveABEA dir

-- Ex 11

data ArvBinGen = Nulo | No Int ArvBinGen ArvBinGen 
 deriving(Show,Eq)

insereGen :: Int -> ArvBinGen -> ArvBinGen
insereGen x Nulo = (No x Nulo Nulo)
insereGen x (No y esq dir)
 |x == y = No y esq dir
 |x < y = No y (insereGen x esq) dir
 |otherwise = No y esq (insereGen x dir)

removeGen :: Int -> ArvBinGen -> ArvBinGen
removeGen val Nulo = Nulo
removeGen val (No v esq dir)
 | val < v = No v (removeGen val esq) dir
 | val > v = No v esq (removeGen val dir)
 | esq == Nulo = dir
 | dir == Nulo = esq
 | otherwise = juntaGen esq dir

juntaGen :: ArvBinGen -> ArvBinGen -> ArvBinGen 
juntaGen esq dir = No maior esq novadir
 where
  maior = maiorGen esq
  novadir = removeGen maior dir

maiorGen :: ArvBinGen -> Int
maiorGen Nulo = undefined
maiorGen (No x esq dir)
 | dir == Nulo = x
 | otherwise = maiorGen dir


pertenceABB :: ArvBinGen -> Int -> Bool
pertenceABB Nulo x = False
pertenceABB (No x esq dir) a 
 | x == a = True
 | x > a = pertenceABB esq a
 | x < a = pertenceABB dir a
--Ex 12
data ArvoreBinBanco = Null | Node (Int,String,Int) ArvoreBinBanco ArvoreBinBanco
 deriving(Show)

b1::ArvoreBinBanco
b1 = Node (7,"10/11/99",300) (Node (30,"14/2/23",500) Null Null) (Node (50,"4/5/00",100) Null Null)

insereBanco :: (Int,String,Int) -> ArvoreBinBanco -> ArvoreBinBanco
insereBanco (x,a,b) Null = (Node (x,a,b) Null Null)
insereBanco (x,a,b) (Node (y,c,d) esq dir)
 |x == y = Node (y,c,d) esq dir
 |x < y = Node (y,c,d) (insereBanco (x,a,b) esq) dir
 |otherwise = Node (y,c,d) esq (insereBanco (x,a,b) dir)


listaBanco :: ArvoreBinBanco -> [Int]
listaBanco Null = [] 
listaBanco (Node (x,y,z) esq dir) = listaBanco dir ++ listaBanco esq ++ [x]

sorteio :: [Int] -> Int -> Int
sorteio [] 0 = 0
sorteio xs 0 = 0
sorteio xs n = pegaElemento xs (mod n (length xs))

pegaElemento:: [Int] -> Int -> Int
pegaElemento [] n = 0
pegaElemento xs 0 = ultimoElemento xs 
pegaElemento (y:xs) 1 = y
pegaElemento (x:xs) n = pegaElemento xs (n-1)

ultimoElemento:: [Int] -> Int
ultimoElemento [] = 0
ultimoElemento [x] = x
ultimoElemento (x:xs) = ultimoElemento xs


