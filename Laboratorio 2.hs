conversao :: Float -> (Float,Float,Float)
conversao x = (x,(x * 3.96),(x * 4.45))

bissexto:: Int-> Bool
bissexto x
 |(mod x 400 == 0) = True
 |(mod x 4 == 0) && (mod x 100 /= 0) = True
 |otherwise = False

type Data = (Int, Int, Int)

bissexto2 :: Data -> Bool
bissexto2 (x,y,z) = bissexto z

valida::Data->Bool
valida (d,m,a)
 |d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) = True
 |d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
 |d >= 1 && d <= 28 && m == 2 && not (bissexto a) = True
 |d >= 1 && d <= 29 && m == 2 && (bissexto a) = True
 |otherwise = False

procede :: Data -> Data -> Bool
procede (d1,m1,a1) (d2,m2,a2)
 | a1 > a2 = False
 | a1 < a2 = True
 | m1 > m2 = False
 | m1 < m2 = True 
 | d1 > d2 = False
 | d1 < d2 = True
 | otherwise = False

type Livro = (String, String, String, String, Int)
type Aluno = (Int, String, String, String)
type Emprestimo = (String, String, Data, Data, String)

e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")

emprestimoEmDia :: Emprestimo ->  Data -> Bool
emprestimoEmDia (_,_,_,(d1,m1,a1),_) (d2,m2,a2) = procede (d2,m2,a2) (d1,m1,a1) 

