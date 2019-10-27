module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ ini  []    = ini
foldl f ini (x:xs) = foldl f (f ini x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ ini  []    = ini
foldr f ini (x:xs) = f x $ foldr f ini xs

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = case f ini of
                  Just (a,ini) -> a : unfoldr f ini
                  Nothing      -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldl (*) 1

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f [] where
  f (Just x) xs = x : xs
  f Nothing  xs = xs

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal = reverse . fst . foldl (\(ds, n) xs -> (xs !! n : ds, n+1)) ([], 0)

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr (\ x b  -> if f x then b else x : b) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem k = foldl (\ b x -> x == k || b) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr f from where
  f x = if x > to then Nothing else Just (x, x + step) 

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append a b = foldr (:) b a

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Int -> [[a]]
groups lst n = unfoldr f lst where
  f [] = Nothing
  f xs = Just $ splitAt n xs


main :: IO ()
main = do
  putStrLn "          -- Suite 1 --"
  let res1 = foldl (/) 64 [4,2,4]
  putStrLn $ "foldl (/) 64 [4,2,4]  -> " ++ show res1
  putStrLn "\n          -- Suit 2 --"
  let res2 = foldr (/) 64 [4,2,4]
  putStrLn $ "foldr (/) 64 [4,2,4]  -> " ++ show res2
  putStrLn "\n          -- Suit 3 --"
  let res3 = unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
  putStrLn $ "unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10  -> " ++ show res3
  putStrLn "\n          -- Suit 4 --"
  let res4 = map (+1) [1..10]
  putStrLn $ "map (+1) [1..10]  -> " ++ show res4
  putStrLn "\n          -- Suit 5 --"
  let res5 = product [1..5]
  putStrLn $ "product [1..5]    -> " ++ show res5
  putStrLn "\n          -- Suit 6 --"
  let res6 = catMaybes [Just 5, Nothing, Just 34, Nothing, Nothing, Just 39]
  putStrLn $ "catMaybes [Just 5, Nothing, Just 34, Nothing, Nothing, Just 39]  -> " ++ show res6
  putStrLn "\n          -- Suit 7 --"
  let res7 = diagonal [[1,2,3],[4,5,6],[7,8,9]]
  putStrLn $ "diagonal [[1,2,3],[4,5,6][7,8,9]] -> " ++ show res7
  putStrLn "\n          -- Suit 8 --"
  let res8 = filterNot even [1,2,3,4,5,6,7,8]
  putStrLn $ "filterNot even [1,2,3,4,5,6,7,8]  -> " ++ show res8
  putStrLn "\n          -- Suit 9 --"
  let res9_1 = elem 5 [1,2,3,4,5,6,7,8]
  putStrLn $ "elem 5 [1,2,3,4,5,6,7,8]   -> " ++ show res9_1
  let res9_2 = elem 15 [1,2,3,4,5,6,7,8]
  putStrLn $ "elem 15 [1,2,3,4,5,6,7,8]  -> " ++ show res9_2
  putStrLn "\n          -- Suit 10 --"
  let res10 = rangeTo 0 11 2
  putStrLn $ "rangeTo 0 11 2    -> " ++ show res10
  putStrLn "\n          -- Suit 11 --"
  let res11 = append [1,2,3] [4,5,6]
  putStrLn $ "append [1,2,3] [4,5,6]    -> " ++ show res11
  putStrLn "\n          -- Suit 12 --"
  let res12 = groups [1,2,3,4,5,6,7,8,9] 4
  putStrLn $ "groups [1,2,3,4,5,6,7,8,9] 4  -> " ++ show res12
