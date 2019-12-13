module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12


instance Functor FourOf where
-- Применяем функцию к каждому из элементов
  fmap f (FourOf a1 a2 a3 a4) = FourOf (f a1) (f a2) (f a3) (f a4)


instance Applicative FourOf where
  pure a = FourOf a a a a

-- Применяем функции из первого аргумента к элементам второго аргумента
  (FourOf f1 f2 f3 f4) <*> (FourOf a1 a2 a3 a4) = FourOf (f1 a1) (f2 a2) (f3 a3) (f4 a4)


instance Monad FourOf where
-- Нюанс в том, что k возвращает 'FourOf a a a a', а нам нужно всего лишь одно значение
-- Следовательно, создаем функции, которые достают нам нужные значения
  (FourOf a1 a2 a3 a4) >>= k = FourOf (first (k a1)) (second (k a2)) (third (k a3)) (fourth (k a4)) where
      first  (FourOf a1 _  _   _) = a1
      second (FourOf _  a2 _   _) = a2
      third  (FourOf _  _  a3  _) = a3
      fourth (FourOf _  _  _  a4) = a4


res  = do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y }
main = res == FourOf 5 8 10 12
