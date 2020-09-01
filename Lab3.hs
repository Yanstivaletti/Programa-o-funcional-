npares :: [Int] -> Int
npares xs = length [x | x <- xs, even x]



boomBangs :: [Int] -> [[Char]]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

listaQuad :: Float -> Float -> [Float]
listaQuad x y = [n**2 | n <-[x..y] ]

listaQuadPares :: Int -> Int -> [Int]
listaQuadPares x y = [n * n | n <-[x..y], even n ]

seleciona_impares :: [Int] -> [Int]
seleciona_impares xs = [n | n <- xs, mod n 2 /= 0]

tabuada :: Int -> [Int]
tabuada x = [n | n <-[x..(10*x)], mod n x == 0]

bissexto:: Int-> Bool
bissexto x
 |(mod x 400 == 0) = True
 |(mod x 4 == 0) && (mod x 100 /= 0) = True
 |otherwise = False

bissextos:: [Int] -> [Int]
bissextos xs = [n | n <-xs, bissexto n == True]

sublistas :: [[Int]] -> [Int]
sublistas xs = [n | z <- xs, n <- z]

precede :: Data -> Data -> Bool
procede (d1,m1,a1) (d2,m2,a2)
 | a1 > a2 = False
 | a1 < a2 = True
 | m1 > m2 = False
 | m1 < m2 = True 
 | d1 > d2 = False
 | d1 < d2 = True
 | otherwise = False

emprestimoEmDia :: Emprestimo ->  Data -> Bool
emprestimoEmDia (_,_,_,(d1,m1,a1),a) (d2,m2,a2)
 |precede (d2,m2,a2) (d1,m1,a1) == False = False
 |a == "encerrado" = False
 |otherwise = True

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo :: Emprestimos 

bdEmprestimo =  [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),   ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),   ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados emp d = [n | n <- emp, emprestimoEmDia n d == False]