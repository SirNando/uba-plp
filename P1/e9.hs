main = print (show (trasponer [[1, 2, 3], [4, 5, 6], [7, 8, 9]]))

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))

trasponer :: [[Int]] -> [[Int]]
trasponer ([] : _) = []
trasponer m = map head m : trasponer (map tail m)