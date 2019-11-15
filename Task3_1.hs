module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber deriving Show

-- Реализуйте все классы типов, которым должны отвечать целые числа

wpnToInt :: WeirdPeanoNumber -> Int
wpnToInt Zero = 0
wpnToInt (Succ x) =   1  + wpnToInt x
wpnToInt (Pred x) = (-1) + wpnToInt x

intToWPN :: Int -> WeirdPeanoNumber
intToWPN x
  | x > 0     = Succ $ intToWPN (x-1)
  | x < 0     = Pred $ intToWPN (x+1)
  | otherwise = Zero

instance Eq WeirdPeanoNumber where
  (==) x y = wpnToInt x == wpnToInt y

instance Ord WeirdPeanoNumber where
  (<=) x y = wpnToInt x <= wpnToInt y

instance Enum WeirdPeanoNumber where
  fromEnum x = undefined
  toEnum   x = undefined

instance Num WeirdPeanoNumber where
  (+)         = undefined
  (*)         = undefined
  abs         = undefined
  signum      = undefined
  fromInteger = undefined
  (-)         = undefined

instance Real WeirdPeanoNumber where
  toRational = undefined

instance Integral WeirdPeanoNumber where
  quotRem   = undefined
  toInteger = undefined

main :: IO ()
main = do
  putStrLn "          -- Suite 1 --"
  let res1 = wpnToInt $ Pred $ Succ $ Succ $ Succ $ Succ Zero
  putStrLn $ "wpnToInt $ Pred $ Succ $ Succ $ Succ $ Succ Zero  -> " ++ show res1
  putStrLn "          -- Suite 2 --"
  let res2 = intToWPN res1
  putStrLn $ "intToWPN res1  -> " ++ show res2
  putStrLn "          -- Suite 3 --"
  let res3 = (Pred $ Succ $ Succ $ Succ $ Succ Zero) == (Succ $ Succ $ Succ Zero)
  putStrLn $ "intToWPN res1  -> " ++ show res3
  putStrLn "          -- Suite 4 --"
  let res4 = (Succ $ Succ $ Succ $ Succ $ Succ Zero) >= (Succ $ Succ $ Succ Zero)
  putStrLn $ "intToWPN res1  -> " ++ show res4
