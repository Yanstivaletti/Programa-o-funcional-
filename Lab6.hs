--Ex1
analisaIMC:: Float -> Float -> String
analisaIMC x y
 | z <= 18.5 = "Voce esta abaixo do ideal"
 | z <= 25.0 = "Seu peso parece normal"
 | z <= 30.0 = "Voce esta acima do peso ideal"
 | z >= 30.0 = "Voce esta obeso"
 | otherwise = "valor invalido"

 where z = x / (y^2) 
--Ex2
analisaIMC2:: Float -> Float -> String
analisaIMC2 x y =
 let a = x / (y^2) in 
 if a <= 18.5 then "Voce esta abaixo do ideal"
 else if a <= 25.0 then "Seu peso parece normal"
 else if a <= 30.0 then "Voce esta acima do peso ideal"
 else if a >= 30.0 then "Voce esta obeso"
 else "valor invalido"
--Ex3
raizes :: Float -> Float -> Float -> (Float,Float,Float)
raizes a b c
 | delta < 0 = (0,0,delta)
 |otherwise = (x1,x2,delta)
 where 
 x1 = (-b + sqrt(b^2 + delta)/2*a)
 x2 = (-b + sqrt(b^2 - delta)/2*a)
 delta = (b^2 - 4 * a *c)
--Ex4
raizes2 :: Float -> Float -> Float -> (Float,Float,Float)
raizes2 a b c =
 let x1 = (-b + sqrt(b^2 + delta)/2*a) ; x2 = (-b + sqrt(b^2 - delta)/2*a);delta = (b^2 - 4 * a *c) in
 if delta < 0 then error "Raizes negativas" 
 else (x1,x2,delta)
--Ex5
pares :: [Int] -> ([Int],Int)
pares [] = ([],0)
pares (h:t) = if (even h) then (h:z, a+1)
 else (z,a)
 where (z,a) = pares t

 