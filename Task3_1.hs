module Task3_1 where

import Data.Tuple

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber deriving Show

-- Реализуйте все классы типов, которым должны отвечать целые числа

wpnToInteger :: WeirdPeanoNumber -> Integer
wpnToInteger Zero = 0
wpnToInteger (Succ x) =   1  + wpnToInteger x
wpnToInteger (Pred x) = (-1) + wpnToInteger x

integerToWPN :: Integer -> WeirdPeanoNumber
integerToWPN x
  | x > 0     = Succ $ integerToWPN $ x-1
  | x < 0     = Pred $ integerToWPN $ x+1
  | otherwise = Zero

instance Eq WeirdPeanoNumber where
  (==) x y = wpnToInteger x == wpnToInteger y

instance Ord WeirdPeanoNumber where
  (<=) x y = wpnToInteger x <= wpnToInteger y

instance Num WeirdPeanoNumber where
  (+) x y     = integerToWPN $ wpnToInteger x + wpnToInteger y
  (*) x y     = integerToWPN $ wpnToInteger x * wpnToInteger y
  abs         = integerToWPN . abs . wpnToInteger
  signum      = signum . fromIntegral . wpnToInteger
  fromInteger = integerToWPN
  (-) x y     = integerToWPN $ wpnToInteger x - wpnToInteger y

instance Enum WeirdPeanoNumber where
  fromEnum = fromInteger . wpnToInteger
  toEnum   = integerToWPN . fromIntegral 

instance Real WeirdPeanoNumber where
  toRational = toRational . wpnToInteger

instance Integral WeirdPeanoNumber where
  toInteger   = wpnToInteger
  quotRem x y = mapTuple integerToWPN ( quotRem (wpnToInteger x) (wpnToInteger y) )
    where
      mapTuple f (x,y) = (f x, f y)


main :: IO ()
main = do
  putStrLn "          -- Suite 1 --"
  let res1 = wpnToInteger $ Pred $ Succ $ Succ $ Succ $ Succ Zero
  putStrLn $ "res1  -> " ++ show res1
  putStrLn "\n          -- Suite 2 --"
  let res2 = integerToWPN res1
  putStrLn $ "integerToWPN res1  -> " ++ show res2
  putStrLn "\n          -- Suite 3 --"
  let res3 = (Pred $ Succ $ Succ $ Succ $ Succ Zero) == (Succ $ Succ $ Succ Zero)
  putStrLn $ "integerToWPN res1  -> " ++ show res3
  putStrLn "\n          -- Suite 4 --"
  let res4 = (Succ $ Succ $ Succ $ Succ $ Succ Zero) >= (Succ $ Succ $ Succ Zero)
  putStrLn $ "integerToWPN res1  -> " ++ show res4
  putStrLn "\n          -- Suite 5 --"
  let res5 =  abs ( integerToWPN (-1) + integerToWPN (-2) * integerToWPN 2 ) - integerToWPN 3
  putStrLn $ "res5  -> " ++ show res5
  putStrLn "\n          -- Suite 6 --"
  let res6 = quotRem ( integerToWPN 11 ) ( integerToWPN 3 )
  putStrLn $ "res6  -> " ++ show res6
