
ffoldl f c (x:xs) = ffoldl f (f x c) xs
ffoldl f c [] = c

ffoldr f c (x:xs) = f x (ffoldr f c xs)
ffoldr f c [] = c

arr = [2, 3, 0, 1, 0]

falses = False : falses

l1 = \ x y -> x && y

