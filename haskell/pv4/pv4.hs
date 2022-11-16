{- 1. Napisati funkciju koja prihvata String i razdvoji ga
   po nekom karakteru. 

   primer: "ana voli milovana" -> "ana", "voli", "milovana" -}

bsplit :: Char -> String -> [String]
bsplit del = foldr (\x (a:as) -> if x /= del then (x : a) : as else [] : a : as) [""]


-- delimiter, token acc, input , list acc 
split :: Char -> String -> [String]
split d s = splitHelper d [] s []

splitHelper :: Char -> String -> String -> [String] -> [String]
splitHelper d token [] acc = acc ++ [token]
splitHelper d token (x:xs) acc
    | x /= d = splitHelper d (token ++ [x]) xs acc
    | otherwise = splitHelper d [] xs (acc ++ [token])


{- 2. Napisati funkciju koja prihvata listu Stringova.
   Sve stringove spoji tako sto izmedju svaka 2 
   umetne karakter ','.

   ["ana", "voli", "milovana"] + ',' -> "ana,voli,milovana" -}

spoji :: [String] -> String
spoji [x] = x
spoji (x:xs) = x ++ "," ++ spoji xs


{- 3. Napisati funkciju koja privhata listu Stringova, 
   razdvoji svaki po razmaku, te ih sve
   spoji zarezima. Koristiti map, fold i prethodne 2 funkcije.

   ["ana voli milovana", "trava je zelena"]
   -> ["ana", "voli", "milovana", "trava", "je", "zelena"]
   -> "ana,voli,milovana,trava,je,zelena" -}

razdvojiSpoji :: [String] -> String
razdvojiSpoji l = spoji $ foldr (++) [] $ map (split ' ') l


{- 4. Napisati funkciju koja prihvata listu listi integer-a ( [[Int]] ).
   Potrebno je prvo kvadrirati elemente svake podliste, zatim ih sumirati.
   Na kraju potrebno je vratiti proizvod te liste.

   svastaSaListom :: Num a => [[a]] -> a

   [[1, -4, 5], [4, 4, 4], [-4, -6, -2]] -> 112896 -}

svastaSaListom :: Num a => [[a]] -> a
svastaSaListom [] = 0
svastaSaListom l = foldr (\x a -> sum (map (^2) x) * a) 1 l


{- 5. Definisati tip podataka naselje. Naselje moze da bude Selo, Varosica ili Grad.
   Sva 3 tipa naselja mogu imati broj stanovnika (integer) i povrsinu (double). 
   Selo nosi informaciju o tome da li je "zbijeno" ili "razbijeno" (string). Grad 
   ima dodatni parametar koji kaze da li sadrzi gradski bazen ili ne (boolean). -}

data Naselje = Selo     { povrsina     :: Double
                        , populacija   :: Int
                        , zbijeno      :: String
                        }
                        |
               Varosica { povrsina     :: Double
                        , populacija   :: Int
                        }
                        |
               Grad     { povrsina     :: Double
                        , populacija   :: Int
                        , gradskiBazen :: Bool
                        } deriving Show


naselja :: [Naselje]
naselja = [ Grad     20000.24   150_500 True
          , Grad     87878.66   149_000 True
          , Grad     87878.66   200_000 False
          , Varosica 929_292    2929
          , Selo     200        123     "razbijeno"
          , Selo     4000       444     "zbijeno"
          , Selo     2000       200     "razbijeno"
          ]

{- 6. Napisati funkciju koja iz liste Naselja izdvaja sva razbijena sela i sve
   gradove sa bazenima koji imaju vise od 150 000 stanovnika. -}

filterNaselja :: [Naselje] -> [Naselje]
filterNaselja = filter jeTrazeno
    where
        jeTrazeno n = jeSelo n && jeRazbijeno n || jeGrad n && populacija n > 150_000 && gradskiBazen n
        jeSelo (Selo {}) = True
        jeSelo _         = False
        jeGrad (Grad {}) = True
        jeGrad _         = False
        jeRazbijeno x = zbijeno x == "razbijeno" 

