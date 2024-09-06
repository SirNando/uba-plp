main = print (show (elementosEnPosicionesPares [1,2,3,4]))

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares ls = foldr (\(index, num) acc -> if even index then num:acc else acc) [] (zip [1 ..] ls)