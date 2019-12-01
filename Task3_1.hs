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

reduce :: WeirdPeanoNumber -> WeirdPeanoNumber
reduce = integerToWPN . wpnToInteger

instance Eq WeirdPeanoNumber where
  (==) x y = eq_fun (reduce x) (reduce y) where
    eq_fun  Zero     Zero    = True
    eq_fun (Succ x) (Succ y) = eq_fun x y
    eq_fun (Pred x) (Pred y) = eq_fun x y
    eq_fun  _        _       = False

instance Ord WeirdPeanoNumber where
  (<=) x y = ord_fun (reduce x) (reduce y) where
    ord_fun (Succ x) (Succ y)  = ord_fun x y
    ord_fun (Pred x) (Pred y)  = ord_fun x y
    ord_fun (Pred _ ) Zero     = True
    ord_fun  Zero    (Succ _ ) = True
    ord_fun  x        y        = x == y


instance Num WeirdPeanoNumber where
  (+) x y     = reduce $ plus_fun x y  where
    plus_fun Zero y = y
    plus_fun x Zero = x
    plus_fun (Succ x) y = plus_fun x (Succ y)
    plus_fun (Pred x) y = plus_fun x (Pred y)

  negate = reduce . negate_fun where
    negate_fun  Zero    = Zero
    negate_fun (Succ x) = Pred $ negate_fun x
    negate_fun (Pred x) = Succ $ negate_fun x

  signum x = case reduce x of
    (Succ _) -> Succ Zero
    (Pred _) -> Pred Zero
    _        -> Zero

  abs x = if signum x >= Zero then x else negate x

  (*) x y = case signum y of
    Succ Zero -> x + x * Pred y
    Pred Zero -> negate $ x * abs y
    Zero      -> Zero

  fromInteger = integerToWPN

instance Enum WeirdPeanoNumber where
  fromEnum = fromInteger . wpnToInteger
  toEnum   = integerToWPN . fromIntegral 

instance Real WeirdPeanoNumber where
  toRational = toRational . wpnToInteger

instance Integral WeirdPeanoNumber where
  toInteger = wpnToInteger
  quotRem x y 
    | signum x == signum y = result
    | otherwise = (negate $ fst result, snd result)
    where
      result = quotRem_abs (Zero, abs x) (abs y)
      quotRem_abs answer@(quot, rem) y
        | rem >= y = quotRem_abs (quot + 1, rem - y) y
        | otherwise = answer


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
  let res5 = wpnToInteger (abs ( integerToWPN (-1) + integerToWPN (-2) * integerToWPN 2 ) - integerToWPN 3)
  putStrLn $ "res5  -> " ++ show res5
  putStrLn "\n          -- Suite 6 --"
  let res6 =  quotRem ( integerToWPN 11 ) ( integerToWPN 3 )
  putStrLn $ "res6  -> " ++ show res6
