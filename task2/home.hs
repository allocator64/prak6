myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup e lst
	| (fst . head) lst == e	= Just ((snd . head) lst)
	| otherwise				= myLookup e (tail lst)

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldl (\acc e -> acc ++ [(f e)]) []

data Tree a = Leaf | Node a (Tree a) (Tree a)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node v l r) = f v (foldTree f acc l) (foldTree f acc r)

depth :: Tree a -> Int
depth Leaf = 0
depth (Node _ l r) = (max (depth l) (depth r)) + 1
