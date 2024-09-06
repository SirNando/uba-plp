main = print (show (factorial 3))

foldNat :: (a -> a) -> a -> Integer -> a
foldNat f z n
  | n < 0 = error "Natural menor que 0"
  | n == 0 = z
  | otherwise = f (foldNat f z (n - 1))

potencia :: Integer -> Integer -> Integer
potencia n m = foldNat (n *) n (m - 1)

factorial :: Integer -> Integer
factorial n = foldNat (* n) 1 (n - 1)