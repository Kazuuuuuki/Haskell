applyTwice :: (a -> a) -> a -> a
applyTwice g x = g (g x)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x



chain :: Integer -> [Integer]
chain 1 = [1]
chain n 
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)



--map (negate . abs) [5, -3, -6, 7, 3]


fn = ceiling . negate . tan . cos . max 50