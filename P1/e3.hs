import Prelude hiding (elem, filter, map, sum, (++))

main = print (show (sumaAlt2 [1,2,3,4]))

{- Punto 1 -}

sum :: (Num a) => [a] -> a
sum = foldr (+) 0

elem :: (Eq a) => a -> [a] -> Bool
elem e = foldr (\num acc -> (num == e) || acc) False

(++) :: [a] -> [a] -> [a]
(++) l1 l2 = foldr (:) l2 l1

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x acc -> if f x then x : acc else acc) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

{- Punto 2 -}

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if f x y then x else y)

{- Punto 3 -}

sumasParciales :: (Num a) => [a] -> [a]
sumasParciales (x : xs) = scanl (+) x xs

{- Punto 4 -}

sumaAlt :: (Num a) => [a] -> a
sumaAlt ls = foldr (\(index, num) acc -> if odd index then acc + num else acc - num) 0 (zip [0 ..] ls)

{- Punto 5 -}

sumaAlt2 :: (Num a) => [a] -> a
sumaAlt2 [x] = x
sumaAlt2 (x:xs) = if odd (length xs) then sumaAlt2 xs - x else sumaAlt2 xs + x