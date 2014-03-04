type Point = (Double, Double)
pathLength :: [Point] -> Double
pathLength lst = sum (zipWith dist lst (tail lst))
	where
		dist :: Point -> Point -> Double
		dist (x1, y1) (x2, y2) = sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f e lst = foldr (flip f) e (rev lst)
	where
		rev = foldr (\e acc -> acc ++ [e]) []

det :: [[Int]] -> Int
det a = (calc_det (gauss a))
	where
		n :: Int
		n = (length a)

		m :: Int
		m = (length (a !! 0))

		get :: [[Int]] -> Int -> Int -> Int
		get a i j = ((a !! i) !! j)

		do_col :: Int -> Int -> Int -> Int -> [[Int]] -> [Int]
		do_col i j d t a = map(\l -> if l < i + 1 then 0 else (get a j l) * (get a i i) - t * (get a i l)) [0..(m - 1)]

		do_row :: Int -> Int -> [[Int]] -> (Int, [[Int]])
		do_row i d a = (
			d * ((get a i i) ^ (m - i - 1)),
			map(\j -> if j < i + 1 then (a !! j) else (do_col i j d (get a j i) a)) [0..(n - 1)]
			)

		gauss :: [[Int]] -> (Int, [[Int]])
		gauss a = foldl (\acc i -> (do_row i (fst acc) (snd acc))) (1, a) [0..((min n m) - 1)]

		mul :: [[Int]] -> Int
		mul a = foldl (\acc i -> acc * (get a i i)) 1 [0..((min n m) - 1)]

		calc_det :: (Int, [[Int]]) -> Int
		calc_det (0, a) = mul a
		calc_det (d, a) = div (mul a) d
