module HW2 where

import PV2
import Data.Char

{--
1. Definisati funkciju koja prima listu brojeva i ukoliko je
    ona parne duzine, svaki element kvadrira, inace svaki element
    mnozi sa 10. Koristiti funkciju map i lambda funkcije.
--}

z1 :: [Int] -> [Int]
z1 x
    | odd $ length x = map (* 10) x
    | otherwise      = map (^ 2) x

-- z1 x
--     | odd $ length x = map (\ x -> x * 10) x
--     | otherwise      = map (\ x -> x * x) x


{--
2. Napisati funkciju koja prima niz karaktera. Prvo je potrebno
   funkcijom filter izbaciti sva velika slova, a zatim pomocu funkcije
   map pretvoriti sva preostala slova u velika.

   Napomena: Potrebno je u .hs fajl importovati funkciju toUpper
   iz modula Data.Char, na sledeci nacin:
   import Data.Char -- Dodati na pocetak fajla
--}

topper :: [Char] -> [Char]
topper x = map toUpper $ filter (not . isUpper) x


{--
3. Napisati funkciju "primeni" koja prima 3 parametra:
	1) f1 :: [[Int]] -> [[Int]] - funkciju koja prihvata [[Int]] i vraca [[Int]]
	2) f2 :: [[Int]] -> [Int] - funkciju koja prihvata [[Int]] i vraca [Int]
	3) listu ciji su elementi liste brojeva, tj. [[Int]]

	Funkcija treba da primeni funkcije f1 i f2 nad prosledjenom listom
	i vrati rezultat (listu tipa [[Int]] pretvara u [Int], pomocu 
	funkcija f1 i f2).
--}
primeni :: ([[Int]] -> [[Int]]) -> ([[Int]] -> [Int]) -> [[Int]] -> [Int]
primeni f1 f2 x = f2 $ f1 x


{--
4. Definisati funkciju "izbaciParnePaSumiraj" preko funkcije "primeni"
   i funkcija iz zadataka 2 i 3 (sa Vezbi 2), koristeci parcijalnu 
   primenu funkcija.
--}
izbaciParnePaSumiraj :: [[Int]] -> [Int]
izbaciParnePaSumiraj = primeni deleven sumpeglaj


{--
5. Definisati funkciju prosecnaDuzina koja prima niz stringova
   i racuna njihovu prosecnu duzinu pomocu funkcije fold i map.
   Dovoljno je vratiti celobrojnu vrednost (npr. 5 umesto 5.3)
--}
prosecnaDuzina :: [[Char]] -> Int
prosecnaDuzina x = div (sum $ map length x) (length x)
-- prosecnaDuzina x = div (foldl (+) 0 $ map length x) (length x)
