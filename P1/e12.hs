arbolNil = Nil

arbol1 = Bin (Bin Nil 2 Nil) 2 (Bin Nil 2 (Bin Nil 2 Nil))

arbolABBBasico = Bin Nil 2 Nil

arbolABB = Bin (Bin Nil 3 (Bin Nil 4 Nil)) 5 (Bin (Bin Nil 6 (Bin Nil 7 Nil)) 10 (Bin Nil 15 Nil))

arbolNoABB = Bin (Bin Nil 6 (Bin Nil 4 Nil)) 5 (Bin (Bin Nil 6 (Bin Nil 7 Nil)) 10 (Bin Nil 15 Nil))

main = do
  print (esABB arbolABB)

data AB a = Nil | Bin (AB a) a (AB a)

-- recursion estructural
foldAB ::
  b -> -- handle Nil
  (b -> a -> b -> b) -> -- handle Bin
  AB a -> -- input AB
  b -- result
foldAB cNil cBin Nil = cNil
foldAB cNil cBin (Bin i r d) = cBin (foldAB cNil cBin i) r (foldAB cNil cBin d)

-- recursion primitiva
recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> AB a -> b
recAB cNil _ Nil = cNil
recAB cNil cBin (Bin i r d) = cBin i r d (recAB cNil cBin i) (recAB cNil cBin d)

-- ejercicio en clase mapAB
mapAB :: (a -> b) -> AB a -> AB b
mapAB f = foldAB Nil (\i r d -> Bin i (f r) d)

-- ejercicio en clase AEB
data AEB a = Hoja a | Binario (AEB a) a (AEB a)

foldAEB ::
  (a -> b) -> -- handle Hoja
  (b -> a -> b -> b) -> -- handle Binario
  AEB a -> -- input AEB
  b -- result
foldAEB cHoja cBin (Hoja a) = cHoja a
foldAEB cHoja cBin (Binario i r d) = cBin (foldAEB cHoja cBin i) r (foldAEB cHoja cBin d)

{- Punto 2 -}
esNil :: AB a -> Bool
esNil arbol = case arbol of
  Nil -> True
  (Bin i r d) -> False

altura :: AB a -> Int
altura = foldAB 0 (\i r d -> 1 + max i d)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\i r d -> 1 + i + d)

{- Punto 3 -}
mejorSegun :: (a -> a -> Bool) -> AB a -> a
mejorSegun f (Bin izq r der) = foldAB r (\i r d -> if f r d && f r i then r else if f i r && f i d then i else d) (Bin izq r der)

{- Punto 4 -}
esABB :: (Ord a) => AB a -> Bool
esABB = recAB True (\i r d recIzq recDer -> mayor r i && menor r d && recIzq && recDer)
  where
    mayor _ Nil = True
    mayor n (Bin _ r _) = n > r
    menor _ Nil = True
    menor n (Bin _ r _) = n < r