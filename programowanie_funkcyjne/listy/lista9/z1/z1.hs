{-# LANGUAGE TypeFamilies #-}

import Control.Monad.ST
import Data.STRef
import RefLang

type instance Loc (ST s) = STRef s (Value (ST s))

instance MonadFresh (ST s) where
  freshLoc =  

instance MonadHeap (ST s) where
  heapGet = readSTRef
  heapSet = writeSTRef
