main = print(show(2 + 7))

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x, y) = f x y