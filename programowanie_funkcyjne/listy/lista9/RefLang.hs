{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module RefLang where
import Control.Monad.ST
import Data.STRef
import Control.Monad.Trans.State.Lazy
import Control.Monad.Fail 
import qualified Data.Map as Map

type Var = String

data Expr
  = Var Var           -- zmienne, np. x y z
  | Fun Var Expr      -- lambdy, składnia jak w Haskellu: \ x y z -> 42
  | App Expr Expr     -- aplikacja (jak w Haskellu i OCamlu)
  | Let Var Expr Expr -- let-wyrażenia, np. let x = 42 in f x
  | Ref Expr          -- Tworzenie nowej referencji, np. ref 42
  | Get Expr          -- Dereferencja, np. !x
  | Set Expr Expr     -- Ustawianie wartości komórki pamięci, np. x := 42
  | Seq Expr Expr     -- Sekwencja wyrażeń, np. x := 42; !x
  | Num Integer       -- Stałe liczbowe
  | Inp               -- Wejście (słowo kluczowe input)
  | Out Expr          -- Wyjście: np. output 42
  deriving (Show)

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
--Zadanie 1
type instance Loc (ST s) = STRef s (Value (ST s))

instance MonadFresh (ST s) where 
    freshLoc = newSTRef (VNum 0)


instance MonadHeap (ST s) where 
    heapGet = readSTRef 
    heapSet = writeSTRef 

--Zadanie 2
type instance Loc (MonadFreshStateT m) = Integer

newtype MonadFreshStateT m a = MonadFreshState (StateT Integer m a)
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadFresh (MonadFreshStateT m) where
  freshLoc = MonadFreshState $ do 
    i <- get 
    put (i + 1)
    return i
--Zadanie 3 
type instance Loc (MonadHeapMapT m) = Integer

newtype MonadHeapMapT m a = MonadHeapMap (StateT (Map.Map (Loc (MonadHeapMapT m)) (Value (MonadHeapMapT m))) m a)
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadHeap (MonadHeapMapT m) where 
    heapGet l = MonadHeapMap $ do  
        map <- get
        return $ map Map.! l
    heapSet l v = MonadHeapMap $ do 
        map <- get 
        put $ Map.insert l v map
        return ()




type Env m = Map.Map String (Value m)

lookupEnv :: (MonadFail m) => Env m -> Var -> m (Value m)
lookupEnv env x =
  case Map.lookup x env of
    Nothing -> fail ("Unbound variable " ++ x)
    Just v  -> return v

extendEnv :: Env m -> Var -> Value m -> Env m
extendEnv env x v = Map.insert x v env

eval :: (MonadFail m, MonadFresh m, MonadHeap m) =>
  Env m -> Expr -> m (Value m)
eval env (Var x)   = lookupEnv env x
eval env (Fun x e) = return $ VFun $ \ v -> eval (extendEnv env x v) e
eval env (App e1 e2) = do
  VFun f <- eval env e1
  v <- eval env e2
  f v
eval env (Let x e1 e2) = do
  v <- eval env e1
  eval (extendEnv env x v) e2
eval env (Ref e) = do
  v <- eval env e
  l <- freshLoc
  heapSet l v
  return (VLoc l)
eval env (Get e) = do
  VLoc l <- eval env e
  heapGet l
eval env (Set e1 e2) = do
  VLoc l <- eval env e1
  v <- eval env e2
  heapSet l v
  return v
eval env (Seq e1 e2) = do
  eval env e1
  eval env e2
eval env (Num n) = return $ VNum n
eval env Inp = do 
   i <- inp ()
   return $ VNum i
eval env (Out e) = do
  v <- eval env e
  case v of 
    VNum i -> do 
      out i
      return v 
    _ -> fail "Expresion dont evaluate to integer"



class Monad m => MonadOI m where 
  inp :: () -> m Integer
  out :: Integer -> m ()

instance Monad m => MonadOI m where 
  inp () = return 5
  out i = return ()


instance Monad IO m => MonadOI m where 
  inp () = do 
    s <- getLine
    return (read s :: Integer)
  out i = do 
    putStr (show i)
    return i
--Zadanie 5

type EvalMonad a =  MonadHeapMapT (MonadFreshStateT (IO ))
