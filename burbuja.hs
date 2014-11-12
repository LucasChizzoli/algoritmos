--Algoritmo burbuja--by Lucas

largo :: [Integer] -> Integer
largo [] = 0
largo lista = 1 + largo (tail lista) 

burbuja :: [Integer] -> [Integer]
burbuja lista = bucle (largo lista) lista

bucle :: Integer -> [Integer] -> [Integer]
bucle n [] = []
bucle n lista | n == 0 = []
bucle n lista | n == 1 = lista
bucle n lista | head lista <= head(tail lista) = bucle (n-1) (head lista : bucle (n-1) (tail lista))
bucle n lista | head lista > head(tail lista) = bucle (n-1) (colocar (head lista) (tail lista))
	
colocar :: Integer -> [Integer] -> [Integer]
colocar x [] = [x]
colocar x (y:ys) | x <= y = x : y : ys
colocar x (y:ys) | otherwise = y : colocar x ys
	
	
