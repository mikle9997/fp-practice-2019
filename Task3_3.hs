module Task3_3 where

{-
  Задание 3.3
  Множество на основе предикатов
-}


import Todo(todo)


-- newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так


--             Вставка со слайдов             --
-- Основной закон моноида:
-- x ‘mappend‘ mzero === mzero ‘mappend‘ x === x
--             Конец вставки                  --

-- Добавить один моноид к другому можно разными способами :
--    Конъюнкцией, Дизъюнкцией, Разностью или же Симметричной разностью
-- Но Разность не удовлетворяет основному закону моноида, поэтому реализовывать будем оставшимися способами

newtype PSetOR  a = PSetOR  { contains1 :: a -> Bool }
newtype PSetAND a = PSetAND { contains2 :: a -> Bool }
newtype PSetXOR a = PSetXOR { contains3 :: a -> Bool }

-- Содержится ли искомое хоть в одном из множеств
instance Monoid (PSetOR a) where
  mempty = PSetOR (const False)
  mappend (PSetOR p1) (PSetOR p2) = PSetOR (\x -> p1 x || p2 x)

-- Содержится ли искомое в каждом из множеств
instance Monoid (PSetAND a) where
  mempty = PSetAND (const False)
  mappend (PSetAND p1) (PSetAND p2) = PSetAND (\x -> (&&) (p2 x) (p2 x))

-- Симметричная разность множеств a.k.a XOR
instance Monoid (PSetXOR a) where
  mempty = PSetXOR (const False)
  mappend (PSetXOR p1) (PSetXOR p2) = PSetXOR (\x -> p1 x && not (p2 x) || not (p1 x) && p2 x)



-- Для реализации Functor, требуется задать fmap. Который имеет вид :
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- Основная проблема при реализации - это отсутствие обраной функции, 
--    которую сложно создать, учитывая, что непонятно каким образом задано множество.
--    Если допустить, что множество конечно, то обратную функцию реально найти перебором,
--    но это уже совсем другая история.
--
-- Теоретически это должно выглядеть следующим образом :
--
-- instance Functor PSetOR where
--   fmap f (PSetOR p) = PSetOR (\(f x) -> p x)
--
-- Но так Haskell не умеет
--
-- Также на ум приходит что-то такое :
--    fmap f (PSetOR p) = PSetOR $ f . p
-- Но это тоже не будет работать потому что :
--    p :: a -> Bool
--    f :: a -> b
