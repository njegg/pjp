-- Primeri sa slajdova --

max2 :: Ord a => a -> a -> a
max2 a b
  | a > b = a
  | otherwise = b


fact :: (Eq t, Num t) => t -> t
fact n = tailRecFact n 1 1

tailRecFact :: (Eq t, Num t) => t -> t -> t -> t
tailRecFact n i acc
  | i == n = acc * n
  | otherwise = tailRecFact n (i + 1) (acc * i)

accFact :: (Eq t, Num t) => t -> t -> t -> t
accFact n i acc
  | i == n    = acc * i
  | otherwise = accFact n (i + 1) (acc * i)

prime :: Integral c => c -> Bool
prime 2 = True
prime x =
  let
    limit = (round . sqrt . fromIntegral) x
    hasDivisor x i
      | i > limit = False
      | mod x i == 0 = True
      | otherwise = hasDivisor x (i + 1)
  in not (hasDivisor x 2)


caseFact :: (Eq p, Num p) => p -> p
caseFact n =
  case n of
    1 -> 1
    n -> n * caseFact(n - 1)


isFive :: Integer -> Bool
isFive = (== 5)

maxTuple (a,b)
  | a > b = a
  | otherwise = b


tripletMiddle (_, x, _) = x


repa [] = -1
repa [x] = x
repa (_:xs) = repa xs


contains [] _ = False
contains (x:xs) target
  | x == target = True
  | otherwise = contains xs target

containsNoXS [] _ = False
containsNoXS list target
  | head list == target = True
  | otherwise = containsNoXS (drop 1 list) target


removeTarget [] _ = []
removeTarget (x:xs) target
  | notElem x xs = x : removeTarget xs target -- x ok, vrati x:proveri(xs)
  | otherwise = removeTarget xs target        -- x bad, vrati xs


unique [] = True
unique [x] = True
unique (x:xs) = notElem x xs && unique xs


(!) :: Int -> Int
(!) = fact


lepi :: [[a]] -> [a]
lepi list2D = [element | list1D <- list2D, element <- list1D]

delioci n= [x | x <- [1 .. n], n `mod` x == 0]
prost n= delioci n == [1, n]
prosti k= [x | x <- [2 .. k], prost x]
savrsen n = (sum.delioci) n == 2 * n
savrseni k = [x | x <- [2 .. k], savrsen x]


removeTargetZip xs t = [x | x <- xs, x /= t]


quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort s ++ [x] ++ quickSort g
  where
    s = [sx | sx <- xs, sx <  x]
    g = [gx | gx <- xs, gx >= x]



-- ubaci element k u sortiranu listu
-- lista nakon ubacivanja ostaje sortirana
insert k [] = [k]
insert k (x:xs)
  | k <= x = k : x : xs
  | otherwise = x : insert k xs

-- ubacuj redom elemente u sortiranu listu
insertsort xs = isakum xs []
  where
    isakum [] ss = ss
    isakum (x:xs) ss = isakum xs (insert x ss)


-- spaja dve sortirane liste u sortiranu listu
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge ys (x:xs)

-- merge sort
merges [] = []
merges [x] = [x]
merges xs = merge (merges left) (merges right)
  where
    n = div (length xs) 2
    left = take n xs
    right = drop n xs


quickff [] = []
quickff [x] = [x]
quickff (x:xs) = quickff s ++ [x] ++ quickff g
  where
    s = filter (<x) xs
    g = filter (>=x) xs


-- rezultat se akumulira pri ulazu u rek
foldll o a [] = a
foldll o a (x:xs) = foldll o (o a x) xs

-- rezultat se akumulira pri vracanju
epikSum = foldll (+) 0
epikProd = foldll (*) 1

factFold n = foldll (*) 1 [1..n]

sum'' = foldll (\x ac -> x + ac) 0
-- isto sto i:
--  sum list = fold f 0 list where f x y = x + y
--  sum list = fold (+) 0 list


reverseR ls = foldr (\x a -> a ++ [x]) [] ls
reverseL ls = foldl (\a x -> a ++ [x]) [] ls


minfold [] = -1
minfold ls = foldr (\x a -> if x < a then x else a) (head ls) ls

betterFoldMin [] = -1
betterFoldMin (x:xs) = foldr min x (x:xs)

containsFold [] _ = False
containsFold ls t = foldr (\x a -> a || x == t) False ls


mapFoldR f = foldr (\x a -> f x : a) [] -- poslednji se prvi doda, dodavaj na pocetak nove
mapFoldL f = foldl (\a x -> a ++ [f x]) [] -- prvi se prvi dodaje, ostale na kraj


filterFoldR p = foldr (\x a -> if p x then x : a else a) []
filterFoldL p = foldl (\a x -> if p x then a ++ [x] else a) []

nodup [] = []
nodup (x:xs) = x : nodup (filter (/=x) xs)


main :: IO()
main = print "helo"
