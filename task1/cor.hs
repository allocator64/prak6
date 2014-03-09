myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f e lst = (foldr (flip (.) . flip f) d lst) b
