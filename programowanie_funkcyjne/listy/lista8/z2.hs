import Data.Char (toUpper)
import System.IO (isEOF)

data StreamTrans i o a
  = Return a
  | ReadS (Maybe i -> StreamTrans i o a)
  | WriteS o (StreamTrans i o a)

toLower :: StreamTrans Char Char a
toLower =
  ReadS
    ( \x ->
        case x of
          Nothing -> toLower
          Just x -> WriteS (toUpper x) (toLower)
    )

runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans (Return a) = do
  return a
runIOStreamTrans (ReadS f) = do
  end <- isEOF
  if end
    then runIOStreamTrans (f Nothing)
    else do
      x <- getChar
      runIOStreamTrans $ f (Just x)
runIOStreamTrans (WriteS o s) = do
  putChar o
  runIOStreamTrans s

--zadanie 3
listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans (Return a) _ = ([], a)
listTrans (WriteS o s) i = (o :: res, a)
  where
    (res, a) = listTrans s i
