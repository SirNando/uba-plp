main = print (show (desdeHasta 1 5))

genLista :: a -> (a -> a) -> Integer -> [a]
genLista _ _ 0 = []
genLista cabeza sig tam = cabeza:genLista (sig cabeza) sig (tam-1)

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta piso techo = genLista piso (1 +) (techo - piso + 1)