arbol1 = Bin (Bin (Bin (Hoja 2) (Hoja 3)) (Hoja 3)) (Hoja 3)

main = print (size arbol1)

data AIH a = Hoja a | Bin (AIH a) (AIH a)

foldAIH ::
  (a -> b) -> -- handle Hoja
  (b -> b -> b) -> -- handle Bin
  AIH a -> -- input AIH
  b -- result
foldAIH cHoja _ (Hoja a) = cHoja a
foldAIH cHoja cBin (Bin i d) = cBin (foldAIH cHoja cBin i) (foldAIH cHoja cBin d)

altura :: AIH a -> Integer
altura = foldAIH (const 0) (\i d -> 1 + max i d)

size :: AIH a -> Integer
size = foldAIH (const 1) (+)