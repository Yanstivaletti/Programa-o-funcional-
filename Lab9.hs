--1
data Temperatura = Quente | Frio
 deriving(Show)
data Estacao = Primavera | Verao | Outono | Inverno
 deriving(Show,Eq)

data Meses = Jan | Fev | Mar | Abr | Mai | Jun | Jul| Ago | Set | Out | Nov | Dez
 deriving (Eq, Show, Enum)

estacao :: Meses -> Estacao
estacao x
 | (x == Jan || x == Fev || x ==  Mar) = Verao
 | (x == Abr || x == Mai || x == Jun) = Outono
 | (x == Jul || x == Ago || x == Set) = Inverno
 | otherwise = Primavera

clima :: Estacao -> Temperatura
clima x
 | (x == Primavera || x == Verao) = Quente
 | otherwise = Frio
--2

data Pessoa1 = Ind Nome1 Sobrenome1 AnoNascimento1


type Nome1 = String
type Sobrenome1 = String
type AnoNascimento1 = Int

p2 = Ind "Yan" "Stivaletti" 1999

primeiroNome1 :: Pessoa1 -> String
primeiroNome1 (Ind pNome _ _) = pNome

ultimoNome1 :: Pessoa1 -> String
ultimoNome1 (Ind _ uNome _ ) = uNome

anoNascimento1 :: Pessoa1 -> Int
anoNascimento1 (Ind _ _ vAno) = vAno
--3
data Pessoa = Id { primNome :: String,
ultNome :: String,
anoNasc :: Int } deriving (Show)

p1::Pessoa
p1 = Id {primNome ="Stephen", ultNome="Hawking", anoNasc=1942}

--4

data Forma = Circulo Float Float Float | Retangulo Float Float Float Float

f1::Forma
f1 = Circulo 10 5 2

area :: Forma -> Float
area (Circulo _ _ x) = pi * x ^ 2
area (Retangulo x1 y1 x2 y2) = (abs (x2 - x1)) *(abs (y2 - y1))

perimetro :: Forma -> Float
perimetro (Circulo x) = 2 * pi * x
perimetro (Retangulo x y) = 2 * x + 2 * y

--5
data Ponto = Pt Float Float
 deriving (Show)
data Forma = Circulo Ponto Float | Retangulo Ponto Ponto 
 deriving (Show)

pt1::Ponto
pt1 = Pt 2 3
f1::Forma
f1 = Circulo pt1 2


area :: Forma -> Float
area (Circulo _ r) = pi * r ^ 2
area (Retangulo (Pt x1 y1) (Pt x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

move:: Forma -> Float -> Float -> Forma
move (Circulo (Pt x y) r) a b = Circulo (Pt (x+a) (y+b)) r
move (Retangulo (Pt x1 y1) (Pt x2 y2)) a b =
	Retangulo (Pt (x1+a) (y1+b)) (Pt (x2+a) (y2+b))

baseCirculo :: Float -> Forma
baseCirculo r = Circulo (Pt 0 0) r

baseRetang :: Float -> Float -> Forma
baseRetang x y = Retangulo (Pt 0 0) (Pt x y)

--Ex6

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

data Expr = Lit Int |Add Expr Expr |Sub Expr Expr
eval::Expr->Inteval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Div e1 e2) = (eval e1) \ (eval e2)

--Ex7 
data ArvoreBinInt = Nulo | No Int ArvoreBinInt ArvoreBinInt
 deriving(Show)
emOrdem :: ArvoreBinInt -> [Int]
emOrdem Nulo = []
emOrdem (No x esq dir) = (emOrdem esq) ++ [x] ++ (emOrdem dir)

preOrdem :: ArvoreBinInt -> [Int]
preOrdem Nulo = []
preOrdem (No x esq dir) = [x] ++ (preOrdem esq) ++ (preOrdem dir)
--Ex8
instance Show a => Show (ArvBinGen a) where
	show Nulo = "_"
	show (No x esq dir) = "{" ++ show x ++ ":" ++ show esq ++ "|" ++ show dir ++ "}"

insereGen :: (Ord a) => a -> ArvBinGen a -> ArvBinGen a
insereGen x Nulo = (No x Nulo Nulo)
insereGen x (No y esq dir)
 |x == y = No y esq dir
 |x < y = No y (insereGen x esq) dir
 |otherwise = No y esq (insereGen x dir)

removeGen :: ord a => a -> ArvBinGen a->ArvBinGen a
removeGen val Nulo = Nulo
removeGen val (No v esq dir)
 | val < v = No v (removeGen val esq) dir
 | val > v = No v esq (removeGen val dir)
 | esq == Nulo = dir
 | dir == Nulo = esq
 | otherwise = juntaGen esq dir

juntaGen :: Ord a => ArvBinGen a -> ArvBinGen -> ArvBinGen a
juntaGen esq dir = No menor esq novadir
 where
 	menor = menorGen dir
 	novadir - removeGen menor dir

menorGen :: Ord a => ArvBinGen a->a
menorGen Nulo = undefined
menorGen (No x esq dir)
 | esq == Nulo = x
 | otherwise = menorGen esq