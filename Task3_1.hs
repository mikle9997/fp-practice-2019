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

-- Ord Enum Show Num Real Real
-- instance Eq WeirdPeanoNumber where

main :: IO ()
main = do
  putStrLn "          -- Suite 1 --"
  let res1 = wpnToInt $ Pred $ Succ $ Succ $ Succ $ Succ Zero
  putStrLn $ "wpnToInt $ Pred $ Succ $ Succ $ Succ $ Succ Zero  -> " ++ show res1
  putStrLn "          -- Suite 2 --"
  let res2 = intToWPN res1
  putStrLn $ "intToWPN res1  -> " ++ show res2
