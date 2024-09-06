arbolNil = Nil

arbol1 = Bin (Bin Nil 1 Nil) 2 (Bin Nil 3 (Bin Nil 4 Nil))

arbol1Espejado = Bin (Bin (Bin Nil 4 Nil) 3 Nil) 2 (Bin Nil 1 Nil)

arbolABBBasico = Bin Nil 2 Nil

arbolABB = Bin (Bin Nil 3 (Bin Nil 4 Nil)) 5 (Bin (Bin Nil 6 (Bin Nil 7 Nil)) 10 (Bin Nil 15 Nil))

arbolNoABB = Bin (Bin Nil 6 (Bin Nil 4 Nil)) 5 (Bin (Bin Nil 6 (Bin Nil 7 Nil)) 10 (Bin Nil 15 Nil))

main = do
  print (espejo arbol1)

data AB a = Nil | Bin (AB a) a (AB a) deriving (Eq, Show)

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

esNil :: AB a -> Bool
esNil arbol = case arbol of
  Nil -> True
  (Bin i r d) -> False

{- Punto 1 -}
-- ramas :: AB a -> [[a]]
-- ramas (Bin i raiz d) = recAB [[raiz]] (\i r d recI recD -> zip r (recI ++ recD)) (Bin i raiz d) -- no esta muy claro que es lo que tiene que hacer esta funcion

cantHojas :: AB a -> Integer
cantHojas = foldAB 1 (\i r d -> i + d)

espejo :: AB a -> AB a
espejo = recAB Nil (\i r d recI recD -> Bin recD r recI)

{- Punto 2 -}

hijoIzquierdo :: AB a -> AB a
hijoIzquierdo (Bin i _ _) = i

hijoDerecho :: AB a -> AB a
hijoDerecho (Bin _ _ d) = d

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura = foldAB esNil (\i _ d arbol -> not (esNil arbol) && i (hijoIzquierdo arbol) && d (hijoDerecho arbol))