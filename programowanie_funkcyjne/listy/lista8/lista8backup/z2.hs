import Control.Monad
import qualified Data.Char as Low
import Data.Maybe
import Distribution.Simple.Utils (xargs)
import Distribution.Types.IncludeRenaming (IncludeRenaming (IncludeRenaming))
import System.IO (isEOF)
import Text.Parsec (parse)

-- Zadanie 1
echoLower :: IO ()
echoLower = do
  end <- isEOF
  if end
    then return ()
    else do
      x <- getChar
      putChar $ Low.toLower x
      echoLower

-- Zadanie 2
data StreamTrans i o a
  = Return a
  | ReadS (Maybe i -> StreamTrans i o a)
  | WriteS o (StreamTrans i o a)

toLower :: StreamTrans Char Char a
toLower =
  ReadS
    ( \i -> case i of
        Nothing -> toLower
        Just i -> WriteS (Low.toLower i) toLower
    )

runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans (Return a) = return a
runIOStreamTrans (ReadS f) = do
  end <- isEOF
  if end
    then runIOStreamTrans (f Nothing)
    else do
      x <- getChar
      runIOStreamTrans (f (Just x))
runIOStreamTrans (WriteS o t) = do
  putChar o
  runIOStreamTrans t

runToLower :: IO a
runToLower = runIOStreamTrans toLower

-- Zadanie 3

listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans (Return a) _ = ([], a)
listTrans (WriteS o t) is = (o : os, a)
  where
    (os, a) = listTrans t is
listTrans (ReadS f) [] = listTrans (f Nothing) []
listTrans (ReadS f) (x : xs) = listTrans (f (Just x)) xs

abc = take 3 $ fst $ listTrans toLower ['A' ..]

aBc = fst $ listTrans toLower ['a', 'B', 'c']

-- Zadanie 4
runCycle :: StreamTrans a a b -> b
runCycle (Return b) = b
runCycle (WriteS o (Return b)) = b
runCycle (WriteS o (WriteS oo t)) = runCycle (WriteS oo t)
runCycle (WriteS o (ReadS f)) = runCycle (f (Just o))
runCycle (ReadS f) = runCycle $ f Nothing

app :: a -> [a] -> [a]
app x = foldr (:) [x]

mcycle :: [a] -> StreamTrans a a b -> b
mcycle _ (Return b) = b
mcycle [] (ReadS f) = mcycle [] $ f Nothing
mcycle (x : xs) (ReadS f) = mcycle xs $ f (Just x)
mcycle xs (WriteS x t) = mcycle (app x xs) t --troszkę wolna, przydałaby się kolejka fifo

runCycle2 :: StreamTrans a a b -> b
runCycle2 = mcycle []

-- Zadanie 5
(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
_ |>| (Return b) = Return b
at |>| (WriteS o bt) = WriteS o $ at |>| bt
WriteS m at |>| (ReadS f) = at |>| f (Just m)
Return a |>| (ReadS f) = Return a |>| f Nothing
ReadS f |>| bt =
  ReadS
    ( \i ->
        case i of
          Nothing -> f Nothing |>| bt
          Just x -> f (Just x) |>| bt
    )

-- Zadanie 6
catchOutputHelper :: [o] -> StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutputHelper os (Return a) = Return (a, os)
catchOutputHelper os (ReadS f) =
  ReadS
    ( \x ->
        case x of
          Nothing -> catchOutputHelper os $ f Nothing
          Just x -> catchOutputHelper os $ f (Just x)
    )
catchOutputHelper os (WriteS o t) = catchOutputHelper (app o os) t

-- outputy są na liście w kolejności odwrotnej do kolejności wypisania
catchOutput :: StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutput = catchOutputHelper []

--zadanie 9
instance Functor (StreamTrans i o) where
  fmap f m = m >>= return . f

instance Applicative (StreamTrans i o) where
  pure = return
  (<*>) = ap

instance Monad (StreamTrans i o) where
  return = Return
  Return a >>= f = f a
  ReadS l >>= f =
    ReadS
      ( \x ->
          case x of
            Nothing -> l Nothing >>= f
            Just x -> l (Just x) >>= f
      )
  WriteS o t >>= f = WriteS o $ t >>= f

-- Zadanie 7
data BF
  = MoveR
  | MoveL
  | Inc
  | Dec
  | Output
  | Input
  | While [BF]
  deriving (Eq)

tokens = ['<', '>', '+', '-', '.', ',', '[', ']']

singleTokens = ['<', '>', '+', '-', '.', ',']

parseToken :: Char -> BF
parseToken '<' = MoveL
parseToken '>' = MoveR
parseToken '+' = Inc
parseToken '-' = Dec
parseToken '.' = Output
parseToken ',' = Input

el :: Char -> [Char] -> Bool
el c [] = False
el c (x : xs) = c == x || el c xs

parseTokens =
  ReadS
    ( \x ->
        case x of
          Nothing -> Return ()
          Just x -> if el x tokens then WriteS x parseTokens else parseTokens
    )

parser wh =
  ReadS
    ( \x ->
        case x of
          Nothing -> if wh then error "unclosed ]" else Return ()
          Just x ->
            if el x singleTokens
              then WriteS (parseToken x) $ parser wh
              else
                if x == '['
                  then whileParser wh
                  else if wh && x == ']' then Return () else error "unexpected ]"
    )

whileParser wh = do
  bfs <- catchOutput (parser True)
  WriteS (While $ snd bfs) (parser wh)

brainFuckParser :: StreamTrans Char BF ()
brainFuckParser = parseTokens |>| parser False

z = take 8 $ fst $ listTrans brainFuckParser ['<', '[', '.', '+', '[', '+', ']', ']', 'a', '.', '.']
