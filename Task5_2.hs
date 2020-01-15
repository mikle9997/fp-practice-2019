module Task5_2 where

import Todo(todo)
import Prelude hiding (concat)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a] deriving (Show)

-- Реализуйте экземпляры классов Show и Eq для этого типа

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

concat :: Zipper a -> Zipper a -> Zipper a
concat (Zipper ll lr) (Zipper rl rr) = Zipper (ll ++ lr) (rl ++ rr)

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt 0    (Zipper ll lr) (Zipper rl rr) = Zipper (rl ++ reverse (ll ++ lr)) rr
insertManyAt index what           into          = insertManyAt (index - 1) what (goRight into)

subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper 0    0  (Zipper l r) = Zipper l []
subZipper 0    to input        = subZipper 0          (to - 1) (goRight input)
subZipper from to input        = subZipper (from - 1)  to      (removeRight input)

main :: IO ()
main = do
  let zipper_1 = fromList [1,2,3,4]
  let zipper_2 = fromList [5,6,7,8]
  let res1  = concat zipper_1 zipper_2
  putStrLn $ "concat zipper_1 zipper_2         -> " ++ show res1
  let res2  = insertManyAt 1 zipper_1 zipper_2
  putStrLn $ "insertManyAt 2 zipper_1 zipper_2 -> " ++ show res2
  let res3 = subZipper 1 3 zipper_1
  let res31  = goLeft . goLeft $ res3
  putStrLn $ "subZipper 2 3 zipper_1           -> " ++ show res3
