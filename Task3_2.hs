module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a deriving Show

rlistToList :: ReverseList a -> [a]
rlistToList  RNil        = []
rlistToList (RCons xs x) = x : rlistToList xs

listToRList :: [a] -> ReverseList a
listToRList  []    = RNil
listToRList (x:xs) = RCons (listToRList xs) x

--- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance Eq a => Eq (ReverseList a) where
  (==)  RNil         RNil        = True
  (==)  RNil         _           = False
  (==)  _            RNil        = False
  (==) (RCons xs x) (RCons ys y) = x == y && xs == ys

instance Ord a => Ord (ReverseList a) where
  (<=)  RNil         RNil        = True
  (<=)  RNil         _           = True
  (<=)  _            RNil        = False
  (<=) (RCons xs x) (RCons ys y) = x <= y && xs <= ys

instance Monoid (ReverseList a) where
  mempty = RNil
  mappend x    RNil        = x
  mappend RNil y           = y
  mappend xs  (RCons ys y) = RCons (xs `mappend` ys) y

instance Functor ReverseList where
  fmap _ RNil = RNil
  fmap f (RCons ys y) = RCons (fmap f ys) (f y)


main :: IO ()
main = do
  putStrLn "          -- Suite 1 --"
  let res1 = listToRList [1,2,3,4,5]
  putStrLn $ "res1  -> " ++ show res1
  putStrLn "\n          -- Suite 2 --"
  let res2 = rlistToList res1
  putStrLn $ "res2  -> " ++ show res2
  putStrLn "\n          -- Suite 3 --"
  let res3 = res1 == listToRList [1,2,3,4]
  putStrLn $ "res3 : res1 == listToRList [1,2,3,4]  -> " ++ show res3
  putStrLn "\n          -- Suite 4 --"
  let res3 = res1 <= listToRList [1,2,0,3,4,6,7]
  putStrLn $ "res3 : res1 <= listToRList [1,2,0,3,4,6,7]  -> " ++ show res3
  putStrLn "\n          -- Suite 5 --"
  let a = listToRList [1,1,2]
      b = listToRList [3,5]
      c = listToRList [8,13]
  let res5 = a `mappend`  mempty `mappend` b  `mappend` c `mappend` mempty
  putStrLn $ "res5  -> " ++ show res5
  putStrLn "\n          -- Suite 6 --"
  let res6 = fmap (+1) (listToRList [1,2,3])
  putStrLn $ "res6  -> " ++ show res6
