{-# LANGUAGE TypeFamilies #-}
import Control.Monad.ST
import Data.STRef


type family Loc (m :: * -> *) :: *

data Value m
  = VNum Integer
  | VFun (Value m -> m (Value m))
  | VLoc (Loc m)

class Monad m => MonadFresh m where
  -- Tworzy świeżą lokację na stercie. Nowej lokacji nie muszą być
  -- przypisane żadne dane.
  freshLoc :: m (Loc m)

class Monad m => MonadHeap m where
  -- Pobiera wartość ze sterty pod podaną lokacją
  heapGet :: Loc m -> m (Value m)
  -- Ustawia wartość na stercie pod podaną lokacją
  heapSet :: Loc m -> Value m -> m ()

type instance Loc (ST s) = STRef s (Value (ST s))

instance MonadFresh (ST s) where 
    freshLoc = newSTRef (VNum 0)


instance MonadHeap (ST s) where 
    heapGet = readSTRef 
    heapSet = writeSTRef 

