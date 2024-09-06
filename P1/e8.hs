main = print (show (armarPares [1, 2, 3, 4] [5, 6, 7]))

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

armarPares :: [a] -> [b] -> [(a, b)]
armarPares _ [] = []
armarPares [] _ = []
armarPares (x : xs) (y : ys) = (x, y) : armarPares xs ys

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f l1 l2 = mapPares f (armarPares l1 l2)