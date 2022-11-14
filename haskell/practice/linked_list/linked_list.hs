data LinkedList = Null | Node Int LinkedList

-- show implementation --
instance Show LinkedList where
    show :: LinkedList -> String
        -- nice print --
    show xs = "[" ++ printList xs ++ "]"
        where
            printList :: LinkedList -> String
            printList Null = ""
            printList (Node x Null) = show x
            printList (Node x next) = show x ++ ", " ++ printList next
        -- simple print --
    -- show Null = "Null"
    -- show (Node x xs) = show x ++ " => " ++ show xs
    

-- Insert --
insert :: LinkedList -> Int -> LinkedList
insert l x = Node x l

-- Normal list to LinkedList --
listToLL :: [Int] -> LinkedList
listToLL = foldr Node Null
-- First idea:
-- listToLL [] = Null
-- listToLL (x:xs) = Node x (listToLL xs)
--
-- Better way:
-- listToLL = foldr (\x a -> Node x a) Null
-- (\x a -> Node x a) can be replaced with just 'Node'
    -- f x a = g x a => f = g
    -- f x a = Node x a 
    -- f = Node

main = print (listToLL [4..9])
