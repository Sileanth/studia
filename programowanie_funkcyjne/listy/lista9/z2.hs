{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.ST
import Data.STRef
import Control.Monad.Trans.State.Lazy


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



newtype MonadFreshStateT m a = MonadFreshState (StateT Integer m a)
  deriving (Functor, Applicative, Monad)

type instance Loc (ST s) = STRef s (Value (ST s))

type instance Loc (MonadFreshStateT m) = Integer

instance Monad m => MonadFresh (MonadFreshStateT m) where
  freshLoc = MonadFreshState $ do 
    i <- get 
    put (i + 1)
    return i
