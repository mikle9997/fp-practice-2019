module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)
import Prelude hiding (sin, cos, gcd)


-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = sum $ takeWhile (\x -> x > prec || x < (-prec)) [genEl x n | n <- [1..]] where 
    prec = 1/10000000
    fact = product . flip take [1..]
    genEl x n = (-1) ** fromIntegral (n-1) * x ** fromIntegral (2*n-1) / fromIntegral (fact (2*n-1))

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = sum $ takeWhile (\x -> x > prec || x < (-prec)) [genEl x n | n <- [0..]] where 
    prec = 1/10000000
    fact = product . flip take [1..]
    genEl x n = (-1) ** fromIntegral n * x ** fromIntegral (2*n) / fromIntegral (fact (2*n))

-- наибольший общий делитель двух чисел
-- gcd :: Integer -> Integer -> Integer
gcd x y = last $ findEq (findDiv x) (findDiv y) where
  findDiv n = [x | x <- [1..n], n `mod` x == 0]
  findEq []     ys = []
  findEq (x:xs) ys
    | x `elem` ys = x : findEq xs ys
    | otherwise   =     findEq xs ys

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = 
  not . null . dropWhile (<from) . takeWhile (<=to) $ zipWith (*) [1..] [1..]

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 1 = x
pow x y = x * pow x (y-1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = foldl (\b a -> b && x `mod` a /= 0) True [2..(x-1)]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo





main :: IO ()
main = do
  putStrLn "          -- Suite 1 --"
  let res1 = show $ doesSquareBetweenExist 101 115
  putStrLn $ "doesSquareBetweenExist 101 115 -> " ++ res1
  let res2 = show $ doesSquareBetweenExist 156 170
  putStrLn $ "doesSquareBetweenExist 156 170 -> " ++ res2
  let res3 = show $ doesSquareBetweenExist 10 15
  putStrLn $ "doesSquareBetweenExist 10 15   -> " ++ res3
  let res4 = show $ doesSquareBetweenExist 9 15
  putStrLn $ "doesSquareBetweenExist 9 15    -> " ++ res4
  let res5 = show $ doesSquareBetweenExist 10 16
  putStrLn $ "doesSquareBetweenExist 10 16   -> " ++ res5
  putStrLn "\n          -- Suit 2 --"
  let res1 = show $ pow 5 3
  putStrLn $ "pow 5 3  -> " ++ res1
  let res2 = show $ pow 10 4
  putStrLn $ "pow 10 4 -> " ++ res2
  putStrLn "\n          -- Suit 3 --"
  let res1 = show $ isPrime 5
  putStrLn $ "isPrime 5   -> " ++ res1
  let res2 = show $ isPrime 10
  putStrLn $ "isPrime 10  -> " ++ res2
  let res3 = show $ isPrime 111
  putStrLn $ "isPrime 111 -> " ++ res3
  let res4 = show $ isPrime 113
  putStrLn $ "isPrime 113 -> " ++ res4
  putStrLn "\n          -- Suit 4 --"
  let res1 = show $ sin 10
  putStrLn $ "sin 10  -> " ++ res1
  putStrLn "\n          -- Suit 5 --"
  let res1 = show $ cos 10
  putStrLn $ "cos 10  -> " ++ res1
  putStrLn "\n          -- Suit 6 --"
  let res1 = show $ gcd 144 1014
  putStrLn $ "gcd 144 1014  -> " ++ res1
  let res2 = show $ gcd 144 1024
  putStrLn $ "gcd 144 1024  -> " ++ res2
  let res3 = show $ gcd 256 1024
  putStrLn $ "gcd 256 1014  -> " ++ res3
  let res4 = show $ gcd 256 1013
  putStrLn $ "gcd 256 1013  -> " ++ res4


