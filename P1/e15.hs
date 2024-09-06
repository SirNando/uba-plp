arbol1 = RT [Hoja 5, RT [Hoja 3, Hoja 8, Hoja 22] 6, Hoja 4] 9

main = print (altura arbol1)

data RoseTree a = Hoja a | RT [RoseTree a] a

foldRose ::
  (a -> b) -> -- handle Hoja
  ([b] -> a -> b) -> -- handle RT
  RoseTree a -> -- input RT
  b -- result
foldRose cHoja _ (Hoja a) = cHoja a
foldRose cHoja cRose (RT hijos r) = cRose (map (foldRose cHoja cRose) hijos) r

cantHojas :: RoseTree a -> Int
cantHojas = foldRose (const 1) (\hijos r -> sum hijos)

hojas :: RoseTree a -> [a]
hojas = foldRose (: []) (\hijos _ -> concat hijos)

distancias :: RoseTree a -> [Integer]
distancias = foldRose (const [0]) (\hijos _ -> concatMap (map (1 +)) hijos)

altura :: RoseTree a -> Integer
altura = foldRose (const 0) (\hijos _ -> 1 + maximum hijos)