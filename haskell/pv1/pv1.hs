-- 1. Napisati funkciju koja uklanja poslednji element liste.

rmLast :: [Int] -> [Int]
rmLast [] = []
rmLast [x] = []
rmLast (x:xs) = x : rmLast xs


-- 2. Napisati funkciju koja uklanja pretposlednji element liste.

rmPredp :: [Int] -> [Int]
rmPredp [] = []
rmPredp [x] = [x]
rmPredp [x,y] = [y]
rmPredp (x:xs) = x : rmPredp xs


-- 3. Izracunati n!
-- a) rekurzivno

fac :: Int -> Int
fac 1 = 1
fac n = n * fac (n - 1)

-- b) repno rekurzivno
fac2 n = facTail n 1 

fac :: Int Int -> Int
facTail 1 a = a
facTail n a = facTail (n - 1) (a * n)


-- 4. Napisati funkciju "imaVelikaSlova" koja proverava da li neki string
-- ima velika slova
hasUppercase :: [Char] -> Bool
hasUppercase "" = False
hasUppercase (x:xs)
  | x >= 'A' && x <= 'Z' = True
  | otherwise = hasUppercase xs 


-- 5. Napisati funkciju "spljosti".
-- [1, 2, 2, 2, 3, 3, 4, 5, 5, 5] -> [1, 2, 3, 4, 5]

spljosti :: [Int] -> [Int]
spljosti [] = []
spljosti [x] = [x]
spljosti (x:y:xs)
  | x == y = x : spljosti xs
  | otherwise = x : spljosti (y:xs)


-- 6. Napisati funkciju koja iz liste stringova izbacuje one koji imaju 
-- sva mala slova

rmLowercase :: [[Char]] -> [[Char]]
rmLowercase [] = []
rmLowercase (x:xs)
  | hasUppercase x = x : rmLowercase xs
  | otherwise = rmLowercase xs


-- 7. Napisati funkciju koja kvadrira sve elemente liste 
-- (preko ZF izraza).

sqrAll :: [Int] -> [Int]
sqrAll [] = []
sqrAll l = [x * x | x <- l]


-- 8. Napisati funkciju jeDeljiv, koji prima 2 broja i vraca
-- true ako prvi broj moze da deli drugi.

jeDeljiv :: Int -> Int -> Bool
jeDeljiv a b = mod b a == 0


-- 9. Napisati funkciju jeDeljivSa3 koristeci prethodno definisanu
-- funkciju

jeDeljivSa3 :: Int -> Bool
jeDeljivSa3 = jeDeljiv 3


-- 10. Napisati funkciju filter' f l, koja filtrira elemente liste l pomocu funkcije f.

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs


-- 11. Definisati funkciju deljiviSa3 koristeci prethodne 2 funkcije.

deljiviSa3 :: [Int] -> [Int]
deljiviSa3 = filter' jeDeljivSa3
