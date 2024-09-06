polinomio1 = Suma (Cte 2) (Prod (Cte 3) X)  -- 2 + 3 * x


main = print(evaluar 1 polinomio1 == 5)

data Polinomio a = X    -- variable x
                | Cte a -- constante "a"
                | Suma (Polinomio a) (Polinomio a)  -- suma de dos polinomios
                | Prod (Polinomio a) (Polinomio a)  -- multiplicacion de dos polinomios

foldPoli :: (a->b)              -- caso cte.
            -> b                -- caso X
            -> (b -> b -> b)    -- suma
            -> (b -> b -> b)    -- producto
            -> Polinomio a      -- polinomio de entrada
            -> b                -- resultado
foldPoli fCte x fSuma fProd poly = case poly of
    X -> x
    Cte a -> fCte a
    Suma p1 p2 -> fSuma (foldPoli fCte x fSuma fProd p1) (foldPoli fCte x fSuma fProd p2)
    Prod p1 p2 -> fProd (foldPoli fCte x fSuma fProd p1) (foldPoli fCte x fSuma fProd p2)

evaluar :: Num a => a -> Polinomio a -> a
evaluar valor = foldPoli id valor (+) (*)