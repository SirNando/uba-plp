main = print (show (insertarOrdenado 3 [1,2,4,5,6]))

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: (Eq a) => a -> [a] -> [a]
sacarUna e = recr (\x xs acc -> if x == e then xs else x : acc) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs acc -> if x>e then e:x:xs else x:acc) []