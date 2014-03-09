--all = foldr (&&) True
--product = foldr (*) 1
--length = sum . map (const 1)
--concat :: [[a]] -> [a]
--concat = foldr (++) []

--data Maybe a = Nothing | Just a
--Nothing :: Maybe a

data Tree a = Leaf | Node a (Tree a) (Tree a)
--sumTree :: ()
sumTree Leaf = 0
sumTree (Node v l r) = v + (sumTree l) + (sumTree r)

mapTree _ Leaf = Leaf
mapTree f (Node v l r) = Node (f v) (mapTree f l) (mapTree f r)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node v l r) = f v (foldTree f acc l) (foldTree f acc r)