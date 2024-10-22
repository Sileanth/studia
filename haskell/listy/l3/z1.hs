
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
  (acc_r, Node t_l t_r, leafs_r)

-- >>> flipVals $ Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))
-- Node (Node (Leaf 4) (Leaf 3)) (Node (Leaf 2) (Leaf 1))


