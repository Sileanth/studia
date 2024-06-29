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

type instance Loc (MonadFreshStateT m) = Integer

instance Monad m => MonadFresh (MonadFreshStateT m) where
  freshLoc = MonadFreshState $ do 
    i <- get 
    put (i + 1)
    return i

type instance Loc (MonadHeapMapT m) = Integer


newtype MonadHeapMapT m a = MonadHeapMap (StateT (Map (Loc (MonadHeapMapT m)) (Value (MonadHeapMapT m))) m a)
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadHeap (MonadHeapMapT m) where 
    heapGet l = MonadHeapMap $ do  
        map <- get
        return $ map ! l
    heapSet l v = MonadHeapMap $ do 
        map <- get 
        put $ insert l v map
        return ()

instance Monad m => MonadFail m where 
    fail s = error s


