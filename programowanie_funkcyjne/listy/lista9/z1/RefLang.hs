{-# LANGUAGE TypeFamilies #-}

module RefLang where

import qualified Data.Map as Map

type Var = String

data Expr
  = Var Var -- zmienne, np. x y z
  | Fun Var Expr -- lambdy, składnia jak w Haskellu: \ x y z -> 42
  | App Expr Expr -- aplikacja (jak w Haskellu i OCamlu)
  | Let Var Expr Expr -- let-wyrażenia, np. let x = 42 in f x
  | Ref Expr -- Tworzenie nowej referencji, np. ref 42
  | Get Expr -- Dereferencja, np. !x
  | Set Expr Expr -- Ustawianie wartości komórki pamięci, np. x := 42
  | Seq Expr Expr -- Sekwencja wyrażeń, np. x := 42; !x
  | Num Integer -- Stałe liczbowe
  | Inp -- Wejście (słowo kluczowe input)
  | Out Expr -- Wyjście: np. output 42
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

  -- Ustawia wartość na stercie pod podaną lokacją
  heapSet :: Loc m -> Value m -> m ()

type Env m = Map.Map String (Value m)

lookupEnv :: (MonadFail m) => Env m -> Var -> m (Value m)
lookupEnv env x =
  case Map.lookup x env of
    Nothing -> fail ("Unbound variable " ++ x)
    Just v -> return v

extendEnv :: Env m -> Var -> Value m -> Env m
extendEnv env x v = Map.insert x v env

eval ::
  (MonadFail m, MonadFresh m, MonadHeap m) =>
  Env m ->
  Expr ->
  m (Value m)
eval env (Var x) = lookupEnv env x
eval env (Fun x e) = return $ VFun $ \v -> eval (extendEnv env x v) e
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
eval env Inp = fail "input is not implemented"
eval env (Out e) = do
  v <- eval env e
  fail "output is not implemented"
