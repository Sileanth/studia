module Lista2 where
import Data.List.NonEmpty (unfold)
import GHC.ForeignPtr (newConcForeignPtr)
import Data.Char (ord, chr)

-- Zad 1. (Enigma)
-- Zakładam że inputem są tylko wielkie litery angielskiego alfabetu


data Rotor = Rotor { wiring   :: [Char]
                   , turnover :: [Char] }
  deriving(Show)


genConfigurations :: [Rotor] -> [[Rotor]]
genConfigurations = unfoldStream $ \ old_config -> (newConfig old_config, old_config)
  where
    newConfig ((Rotor (w : ws) turn) : rs)
      | w `elem` turn = Rotor {wiring=ws ++ [w], turnover=turn} : newConfig rs
      | otherwise     = Rotor {wiring=ws ++ [w], turnover=turn} : rs



encode :: [Rotor] -> String -> String
encode rs s = zipWith transform s (genConfigurations rs)
  where
    transform = foldr $ \ rot c -> wiring rot !! (ord c - ord 'A')


-- >>> encode rotors "AAAAAA"
-- "ABCEFG"

elemIndex :: Eq a => a -> [a] -> Int
elemIndex c xs = find xs 0
  where
    find (x : xs) n
      | x == c = n
      | otherwise = find xs (n + 1)

decode :: [Rotor] -> String -> String
decode rs s = zipWith untransform s $ genConfigurations rs
  where
    untransform = foldl $ \ acc rot -> chr (elemIndex acc (wiring rot) + ord 'A')

-- >>> decode rotors "ABCEFG"
-- "AAAAAA"




rotors :: [Rotor]
rotors =
  [ Rotor "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "C"
  , Rotor "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "" ]

rotors1 :: [Rotor]
rotors1 =
  [ Rotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" "Q"
  , Rotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" "E"
  , Rotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" "V"
  , Rotor "ESOVPZJAYQUIRHXLNFTGKDCMWB" "J"
  , Rotor "VZBRGITYUPSDNHLXAWMJQOFECK" "Z"
  , Rotor "JPGVOUMFYQBENHZRDKASXLICTW" "ZM"
  , Rotor "NZJHGRCXMYSWBOUFAIVLPEKQDT" "ZM"
  , Rotor "FKQHTLXOCBJSPDZRAMEWNIUYGV" "ZM" ]

rotors2 :: [Rotor]
rotors2 =
  [ Rotor "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "C"
  , Rotor "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "" ]

-- Zad 2 (unfold)

unfoldStream :: (seed -> (seed, val)) -> seed -> [val]
unfoldStream f s = val : unfoldStream f new_seed
  where (new_seed, val) = f s


-- >>> take 10 $ unfoldStream (\ n -> (n+1, n)) 0 
-- [0,1,2,3,4,5,6,7,8,9]

-- >>> take 10 $ unfoldStream (\(p,q) -> ((q, q+p), p)) (0,1) 
-- [0,1,1,2,3,5,8,13,21,34]

-- Zad 3 (Pascal)

pascal :: [[Integer]]
pascal = unfoldStream gen [1]
  where
    gen xs = (1 : genRight xs, xs) -- Responsible for leftmost 1
    genRight (x1 : xs@(x2 : _)) = x1 + x2 : genRight xs
    genRight xs = xs -- Responsible for rightmost 1, xs = [1]

-- >>> take 6 pascal
-- [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]

-- Zad 4 (generowanie drzew)

data Tree = Leaf | Node Tree Tree

instance Show Tree where
  show Leaf = "."
  show (Node l r) = "(" ++ show l ++ show r ++ ")"


treesN :: Integer -> [Tree]
treesN 0 = [Leaf]
treesN n = concatMap (\k ->
    [ Node l r | l <- treesN k, r <- treesN (n-k-1)])
    [0..(n-1)]

trees :: [Tree]
trees = concatMap treesN [0..]

-- >>> take 8 trees
-- [.,(..),(.(..)),((..).),(.(.(..))),(.((..).)),((..)(..)),((.(..)).)]

-- Zad 5 (lista dwukierunkowa)

data DList a = DCons { val  :: a
                     , prev :: Maybe (DList a)
                     , next :: Maybe (DList a) }

toDList :: [a] -> Maybe (DList a)
toDList [] = Nothing
toDList (x : xs) = Just $ converter Nothing x xs
  where
    converter prev val [] = DCons {val=val, prev=prev, next=Nothing}
    converter prev val (x:xs) =
      let node = DCons {
        val=val,
        prev=prev,
        next= Just $ converter (Just node) x xs}
      in node


bounce :: Int -> DList a -> a
bounce = go prev next where
  go dir anty 0 ds = val ds
  go dir anty n ds = case dir ds of
                       Just ds' -> go dir anty (n-1) ds'
                       Nothing  -> go anty dir n ds


