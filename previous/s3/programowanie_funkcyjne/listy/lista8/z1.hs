import System.IO( isEOF )
import Data.Char(toUpper)

echoLower :: IO ()
echoLower = do
    end <- isEOF
    if end then 
        return ()
    else do
        x <- getChar
        putChar $ toUpper x
        echoLower
