{- 1. Kreirati novi data tip Stablo, koji moze imati elemente Nista
   i Cvor, gde Cvor sadrzi nosi neki Int, kao i levo i desno podstablo
   (koji su tipa Stablo). -}

data Tree = Empty | Node Int Tree Tree deriving Show

tree :: Tree
tree = Node 1 (Node 42 (Node 44 Empty Empty) (Node 99 Empty Empty)) (Node 10 Empty (Node 23 Empty Empty))


{- 2. Napisati funkciju "sadrzi" koja prima Int i Stablo, te pretrazuje
   da li se prosledjeni Int nalazi u stablu. Vraca Boolean vrednost. -}

contains :: Int -> Tree -> Bool
contains t Empty = False
contains t (Node x l r) = x == t || contains t l || contains t r

-- contains t (Node x l r)
--     | x == t = True
--     | otherwise = contains t l || contains t r


{- 3. Napisati funkciju uListu koja sve brojeve u stablu smesta u listu. -}

treeToListPre :: Tree -> [Int]
treeToListPre Empty = []
treeToListPre (Node x l r) = [x] ++ treeToListPre l ++ treeToListPre r

treeToListIn :: Tree -> [Int]
treeToListIn Empty = []
treeToListIn (Node x l r) = treeToListIn l ++ [x] ++ treeToListIn r

treeToListPost :: Tree -> [Int]
treeToListPost Empty = []
treeToListPost (Node x l r) = treeToListPost l ++ treeToListPost r ++ [x] 


{- 4. Napisati funkciju koja vraca listu svih brojeva u stablu
   koji su deljivi sa 3 ili 5. -}

div3or5 :: Tree -> [Int]
div3or5 Empty = []
div3or5 (Node x l r) = [x | mod x 3 == 0 || mod x 5 == 0] ++ div3or5 l ++ div3or5 r

-- div3or5 (Node x l r) = (if mod x 3 == 0 || mod x 5 == 0 then [x] else []) ++  [x] ++ div3or5 l ++ div3or5 r

{- 5. Napisati funkciju preslikaj koja prima stablo i preslika ga
   "u ogledalu". (Obrne levo i desno podstablo). -}

mirror :: Tree -> Tree
mirror Empty = Empty
mirror (Node x l r) = Node x (mirror r) (mirror l)


{- 6. Napisati funkciju filterStablo. Ona kao parametre ocekuje funkciju f
   koja prima Int i vraca Bool, i Stablo. Rezultat funkcije filterStablo
   je lista brojeva iz stabla koji vrate True primenom funkcije f. -}

filterTree :: (Int -> Bool) -> Tree -> [Int]
filterTree f Empty = []
filterTree f (Node x l r) = [x | f x] ++ filterTree f l ++ filterTree f r


-- Tests

filterTreeTest :: Bool
filterTreeTest = filter odd (treeToListPre tree) == filterTree odd tree

mirrorTest :: Bool
mirrorTest = reverse (treeToListIn tree) == treeToListIn (mirror tree)

