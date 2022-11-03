module PV2 where
   
{- 1. Napisati funkciju "ispeglaj" koja prima listu ciji su elementi
   liste [[Int]], a vraca listu brojeva [Int] tako sto sve elemente
   podlisti spoji u jednu listu.

   [[1, 2, 3], [4, 5], [6], [7, 8], [], [9]] -> [1, 2, 3, 4, 5, 6, 7, 8, 9] -}

ispeglaj :: [[Int]] -> [Int]
ispeglaj [] = []
ispeglaj (x:xs) = x ++ ispeglaj xs


{- 2. Napisati funkciju koja prima listu listi brojeva [[Int]], i vraca
   listu suma elemenata listi koje ona sadrzi. Koristiti funkciju fold.

   [[1, 2, 3], [2, 3], [1, 2, 4], [5]] -> [6, 5, 7, 5] -}

sumpeglaj :: [[Int]] -> [Int]
sumpeglaj [] = []
sumpeglaj (x:xs) = [sum x] ++ sumpeglaj xs

{- 3. Napisati funkciju koja prima listu ciji su elementi liste brojeva,
   te iz svake podliste izbacuje parne brojeve. Ako slucajno ostane
   neka prazna lista, nju je potrebno izbaciti. Koristiti funkciju
   filter.

   [[1, 2, 3], [2, 4], [3, 4, 5], [7]] -> [[1, 3], [3, 5], [7]] -}

deleven :: [[Int]] -> [[Int]]
deleven x = rmEmpty $ delevenHelper x

delevenHelper :: [[Int]] -> [[Int]]
delevenHelper [] = []
delevenHelper (x:xs) = filter odd x : deleven xs

rmEmpty :: [[Int]] -> [[Int]]
rmEmpty [] = []
rmEmpty [[]] = []
rmEmpty x = filter (/= []) x


{- 4. Napisati funkciju "okreni" koja prolazi kroz listu stringova, 
   te pomocu funkcija map i reverse "okrene" sve stringove.

   "neki string" -> "gnirts iken" -}

okreni :: [[Char]] -> [[Char]]
okreni [] = []
okreni x = map reverse x


{- 5. Napisati funkciju koja prihvata listu listi brojeva [[Int]] i 
   iz svake podliste izbacuje elemente deljive sa 3. -}

nedeljivi3 :: [[Int]] -> [[Int]]
nedeljivi3 = map (filter (\x -> mod x 3 /= 0))


{- 6. Napisati funkciju koja prihvata listu listi brojeva, primenjuje na 
   nju prethodnu funkciju i onda izbaci sve podliste koje imaju manje od 5 elemenata. -}

dugackeNedeljivi3 :: [[Int]] -> [[Int]] 
dugackeNedeljivi3 x = filter (\x -> length x > 5) (nedeljivi3 x)