module Lista3 where

import Data.Foldable
import qualified Data.List as List
import qualified System.Random as Random

-- Zad 1

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

flipVals :: Tree a -> Tree a
flipVals t = 
  let (leafs, res, _) = aux leafs t []
  in res

aux :: [a] -> Tree a -> [a] -> ([a], Tree a, [a])
aux ~(l : leafs) (Leaf x) acc = (x : acc, Leaf l, leafs)
aux leafs (Node l r) acc = 
  let (acc_l, t_l, leafs_l) = aux leafs l acc in 
  let (acc_r, t_r, leafs_r) = aux leafs_l r acc_l in 
  (acc_r, Node l r, leafs_r)

-- >>> flipVals Node (Node 1 2) (Node 3 4)

-- Zad 2

newtype DiffList a = DiffList { unDiffList :: [a] -> [a] }

fromDiffList :: DiffList a -> [a]
fromDiffList = undefined

toDiffList :: [a] -> DiffList a
toDiffList = undefined

diffSingleton :: a -> DiffList a
diffSingleton = undefined

instance Semigroup (DiffList a) where
  -- all methods undefined

instance Monoid (DiffList a) where
  -- all methods undefined

instance Foldable Tree where
  -- all methods undefined

-- Zad 3

data CoinTree a = CTLeaf a
                | CTNode (CoinTree a) (CoinTree a)

data ProbTree a = PTLeaf a
                | PTNode Double (ProbTree a) (ProbTree a)

toCoinTree :: ProbTree a -> CoinTree a
toCoinTree = undefined

coinRun :: [Bool] -> CoinTree a -> a
coinRun = undefined

randomCoinRun :: CoinTree a -> IO a
randomCoinRun t = do 
  gen <- Random.initStdGen
  let bitStream = List.unfoldr (Just . Random.uniform) gen
  return (coinRun bitStream t)

-- Zad 4

data Frac = Frac Integer Integer

instance Num Frac where
  -- all methods undefined

-- Zad 5

data CReal = CReal { unCReal :: [Frac] }
  
instance Num CReal where
  -- all methods undefinedcle [fromInteger n]

realPi :: CReal
realPi = undefined


