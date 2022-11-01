-- 1. Napisati funkciju koja uklanja svaki N-ti element liste.
rmNth :: [Int] -> Int -> [Int]
rmNth l n = rmNthHelper l n 0

rmNthHelper :: [Int] -> Int -> Int -> [Int]
rmNthHelper [] _ _ = []
rmNthHelper (x:xs) n i
  | n == i + 1 = rmNthHelper(xs) n (i + 1)
  | otherwise = x : rmNthHelper(xs) n (i + 1)


-- 2. Napisati funkciju koja vraca listu delilaca nekog pozitivnog broja.
delioci :: Int -> [Int] 
deliociHelper :: Int -> Int -> [Int] -> [Int]

delioci n = deliociHelper n 1 []

deliociHelper n i l
  | i >= n = l ++ [n]
  | mod n i == 0 = deliociHelper n (i + 1) (l ++ [i])
  | otherwise = deliociHelper n (i + 1) l

-- 3. Napisati quicksort.
qs :: [Int] -> [Int]
qs [] = []
qs [x] = [x]
qs (x:xs) = (qs lx) ++ [x] ++ (qs gx)
  where
    lx = filter (< x) xs
    gx = filter (>= x) xs

-- 4 Definisati filter'' f l preko ZF izraza
filter'' :: (Int -> Bool) -> [Int] -> [Int]
filter'' f l = [x | x <- l, f x]

-- 5. Napisati funkciju koja sumira sve elemente liste.
sum' :: [Int] -> Int
sum' = foldl (+) 0

sum'' :: [Int] -> Int
sum'' :: [Int] -> Int

-- 6. Napisati funkciju zip' koja prima 2 liste i sabira elemente na istim indeksima. 
-- 7. Napisati funkciju koja sumira sve cifre prosledjenog broja:
-- 	a. rekurzivno
-- 	b. repno rekurzivno
-- 8. Sumirati parne cifre nekog broja 
-- 