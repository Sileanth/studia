{-# LANGUAGE TypeFamilies #-}

import Control.Monad.ST
import Data.STRef
import RefLang

type instance Loc (ST s) = STRef s (Value (ST s))

instance MonadFresh (ST s) where
  freshLoc = newSTRef (fail "abc")

instance MonadHeap (ST s) where
  heapGet = readSTRef
  heapSet = writeSTRef
