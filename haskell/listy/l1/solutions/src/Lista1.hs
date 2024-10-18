{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lista1
    ( factorial
    , ack
    , removeVowels
    , everyOtherIn
    , everyOtherEx
    , merge
    , transpose
    , primeFactors
    ) where
import Data.Char (toLower)




-- Zadanie 1
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- >>> factorial 5
-- 120

-- >>> factorial 3
-- 6

-- Zadanie 2
ack :: Integer -> Integer -> Integer -> Integer
ack m n 0 = m + n
ack _m 0 1 = 0
ack _m 0 2 = 1
ack m 0 _p = m
ack m n p = ack m (ack m (n-1) p) (p-1)

-- >>> ack 2 2 3
-- 16

-- >>> ack 2 3 3
-- 65536

-- >>> ack 3 7 0
-- 10

-- >>> ack 3 7 1
-- 21

-- >>> ack 3 7 2
-- 2187



-- Zadanie 3
removeVowels :: String -> String
removeVowels = filter (\x -> toLower x `notElem` vowels)
  where vowels = ['a', 'e', 'i', 'o', 'u', 'y']


-- >>> removeVowels "Ala ma kota"
-- "l m kt"


-- Zadanie 4
everyOtherIn :: [a] -> [a]
everyOtherIn [] = []
everyOtherIn (x : xs) = x : everyOtherEx xs
-- >>> everyOtherIn "Ala ma kota"
-- "Aam oa"

everyOtherEx :: [a] -> [a]
everyOtherEx [] = []
everyOtherEx [_] = []
everyOtherEx (_ : x : xs) = x : everyOtherEx xs
-- >>> everyOtherEx "Ala ma kota"
-- "l akt"

merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) = x : y : merge xs ys
-- >>> merge (everyOtherIn "Ala ma kota") (everyOtherEx "Ala ma kota")
-- "Ala ma kota"

-- >>> merge "Haskell" "Ala ma kota"
-- "HAalsak emlal kota"

-- Zadanie 5

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)
-- >>> transpose ["rok","ala","but","las"]
-- ["rabl","olua","kats"]


-- Zadanie 6
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]


primeFactors :: Integer -> [Integer]
primeFactors n = factors n primes
  where 
    factors 1 _ = []
    factors x (p : prim) =
      if x `mod` p == 0
      then p : factors (x `div` p) (p : prim)
      else factors x prim 

-- >>> take 10 (map primeFactors [1..])
-- [[],[2],[3],[2,2],[5],[2,3],[7],[2,2,2],[3,3],[2,5]]

