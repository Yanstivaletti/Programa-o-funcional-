dobro :: Int -> Int
dobro v0 = 2 * v0

quadruplo :: Int -> Int
quadruplo v0 = 2 * dobro v0

hip :: Float -> Float -> Float
hip c1 c2 = sqrt(c1 ** 2 + c2 ** 2)  

ptomedio :: Float -> Float -> Float -> Float -> Float
ptomedio :: x1 y1 x2 y2 = sqrt((x2 - x1)^2 + (y2 - y1)^2)

ehpar :: Int -> Bool
ehpar x = if  mod x 2 == 0 then True
 else False

ehimpar :: Int -> Bool
ehimpar x =  if ehpar x == False then True
 else False

faren :: Float -> Float
faren x = (x - 32) * 5 / 9

maior2 :: Int -> Int -> Int
maior2 x y = if x >= y then x
                else y

maior3 :: Int -> Int -> Int -> Int
maior3 x y z
  | x >= y && x >= z = x
  | y >= x && y >= z = y
  | otherwise = z

retorno :: Float -> Float
retorno x 
   | x > 0 = 1
   | x < 0 = -1
   | x == 0 = 0

ehDigito :: Char -> Bool
ehDigito c = if c >= '0' && c <= '9' then False
 else True

ladosCCH :: Float -> Float -> Float -> Bool
ladosCCH x y z = if x + y > z then True
 else False