module Task4_1 where

import Control.Monad (ap)

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
newtype FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`


-- Реализованно именно так, потому что :
--    p     :: String -> a
--    f     :: a      -> b
--    f . p :: String -> b
instance Functor FunMonad where
  fmap f (FunMonad p) = FunMonad $ f . p

instance Applicative FunMonad where
  pure x = FunMonad $ const x

-- param1 : FunMonad (String -> a) -> b
-- param2 : FunMonad (String -> a)
-- result : FunMonad (String -> b)
-- Чтобы получить нужный result требуется к param1 применить к 'String' и к 'a'
-- Чтобы получить 'a' нужно применить param2 к 'String'
  (FunMonad f) <*> (FunMonad x) = FunMonad $ \ str -> f str (x str)



instance Monad FunMonad where
  -- return a = todo
  -- fail = todo
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- param1 : FunMonad (String -> a)
-- param2 : a -> FunMonad (String -> b)
-- result : FunMonad (String -> b)
-- Чтобы получить result нужно получить 'b'
-- Чтобы получить 'b' нужно вызвать fun для param2 применённого к 'a'
--     и применить результат fun к 'String'
-- Чтобы получить 'a' нужно применить param1 к 'String'
  (FunMonad x) >>= k = FunMonad $ \ str -> fun (k $ x str) str
