{- 1. Kreirati data tip, koji kao moguce vrednosti moze biti ili prazan
   element ili Cvor koji sadrzi informaciju i referencu na sledeci
   element u listi.  -}

data Element a = Prazan | Cvor a (Element a) deriving Show


{- 2. Napisati funkciju "kreirajMojuListu" koja prima listu brojeva
   i od njenih elemenata kreira listu pomocu gore definisanog
   tipa podataka.  -}

kreirajMojuListu :: [Int] -> Element Int
kreirajMojuListu [] = Prazan
kreirajMojuListu (x:xs) = Cvor x (kreirajMojuListu xs) 

kreirajMojuListuPlaneta :: [Planeta] -> Element Planeta
kreirajMojuListuPlaneta [] = Prazan
kreirajMojuListuPlaneta (x:xs) = Cvor x (kreirajMojuListuPlaneta xs) 

{- 3. Napisati funkciju "duzinaListe" koja prima gore kreiranu listu
   i vrati njenu duzinu.  -}

duzinaListe :: Element a -> Int
duzinaListe Prazan = 0
duzinaListe (Cvor x xs) = 1 + duzinaListe xs


{- 4. Napisati funkciju "uListi" koja prima element i gore definisanu
   listu, te vraca Boolean u zavisnosti od toga da li se element
   nalazi u listi ili ne.  -}

uListi :: Eq a => a -> Element a -> Bool
uListi _ Prazan = False
uListi t (Cvor x xs)
  | x == t = True
  | otherwise = uListi t xs


{- 5. Definisati data tip Planeta, koji moze imati vrednosti Nista
   ili slog koji sadrzi polja za ime :: String, precnik :: Double
   i gasovita :: Boolean.  -}

data Planeta = Nista | Planeta {
  ime :: String,
  precnik :: Double,
  gasovita :: Bool
} deriving Show


{- 6. Definisati tip podataka Planete kao listu planeta.  -}

type Planete = Element Planeta

p = kreirajMojuListuPlaneta [Planeta "P1" 2.4 False, Planeta "Trazena" 4.4 True, Planeta "Epik planeta" 43.33 True]

{- 7. Napisati funkciju "nadjiPoImenu" koja prima String i Planete i 
   vraca planetu sa datim imenom. U slucaju da je ne nadje, vraca
   Nista (definisano u data tipu Planeta).  -}

nadjiPoImenu :: String -> Planete -> Planeta
nadjiPoImenu _ Prazan = Nista
nadjiPoImenu trazeno (Cvor x xs)
  | ime x == trazeno = x
  | otherwise = nadjiPoImenu trazeno xs


{- 8. Napisati funkciju "vratiGasovite" koja prima Planete i vraca
   Planete, ali samo one koje su gasovite.  -}

vratiGasovite :: Planete -> Planete
vratiGasovite Prazan = Prazan
vratiGasovite (Cvor x xs)
  | gasovita x = Cvor x (vratiGasovite xs)
  | otherwise = vratiGasovite xs
