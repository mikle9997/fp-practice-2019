module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)


data Operator = Plus | Minus | Mult deriving (Eq)

instance Show Operator where
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"


data Term = IntConstant{ intValue :: Int }                            -- числовая константа
            | Variable{ varName :: String }                           -- переменная
            | BinaryTerm{ op :: Operator, lhv :: Term, rhv :: Term }  -- бинарная операция
            deriving(Eq)

instance Show Term where
  show (IntConstant intValue)  = show intValue
  show (Variable varName)      = varName
  show (BinaryTerm op lhv rhv) = show lhv ++ " " ++ show op ++ " " ++ show rhv


-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 6 |-|
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 7 |*|
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Mult

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (BinaryTerm op lhv rhv) = let 
    recursiveCall = replaceVar varName replacement
  in BinaryTerm op (recursiveCall lhv) (recursiveCall rhv)
replaceVar varName replacement (Variable name) 
  | name == varName = replacement
  | otherwise       = Variable name
replaceVar _ _ expression = expression


-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate input@(BinaryTerm op lhv rhv) = let
    parseOp :: Operator -> Int -> Int -> Int
    parseOp Plus  x y = x + y
    parseOp Minus x y = x - y
    parseOp Mult  x y = x * y
  in case (evaluate lhv, evaluate rhv) of
    (IntConstant const1, IntConstant const2) -> IntConstant $ parseOp op const1 const2
    (_,_) -> input
evaluate expression = expression


main :: IO ()
main = do
  let x    = Variable "loveWAR" |-| IntConstant 3 |*| IntConstant 1 |+| Variable "someVAR"
  let res1 = replaceVar "loveWAR" (Variable "hateWAR") x
  putStrLn $ "res1 : " ++ show res1
  let foo a b c = IntConstant a |+| IntConstant b |-| IntConstant c
  let res2      = foo 5 2 3
  putStrLn $ "res2 : " ++ show res2
  let res3 = evaluate res2
  putStrLn $ "res3 : " ++ show res3
