module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)
import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree | 
                 Leaf {key :: Integer, value :: v} |
                 Node {key :: Integer, value :: v, lhv :: TreeMap v, rhv :: TreeMap v} 
                 deriving Show

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains  EmptyTree           k = False
contains (Leaf key _ )        k = key == k
contains (Node key _ lhv rhv) k = key == k || contains lhv k || contains rhv k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> Maybe v
lookup k  EmptyTree = Nothing
lookup k (Leaf key value )
  | key == k  = Just value
  | otherwise = Nothing
lookup k (Node key value lhv rhv)
  | key == k  = Just value
  | otherwise = case (lookup k lhv, lookup k rhv) of
                      (Just x, _ ) -> Just x
                      ( _ , Just x) -> Just x
                      ( _ , _ ) -> Nothing

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k,v)  EmptyTree         = Leaf k v
insert (k,v) (Leaf key value)  = Node key value (Leaf k v) EmptyTree
insert (k,v) (Node key value lhv rhv)
    | lDepth > rDepth = Node key value lhv (insert (k,v) rhv)
    | otherwise       = Node key value (insert (k,v) lhv) rhv
    where
      lDepth = calcDepth lhv
      rDepth = calcDepth rhv

calcDepth :: TreeMap v -> Int
calcDepth  EmptyTree         = 0
calcDepth (Leaf _ _ )        = 1
calcDepth (Node _ _ lhv rhv) = 1 + calcDepth lhv + calcDepth rhv

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i EmptyTree = EmptyTree
remove i x@(Leaf key value)
  | key == i  = EmptyTree
  | otherwise = x
remove i (Node key value lhv rhv)
  | key == i  = insertLtoR lhv rhv
  | otherwise = Node key value (remove i lhv) (remove i rhv)
  where
    insertLtoR basic EmptyTree                = basic
    insertLtoR basic (Leaf key value)         = insert (key,value) basic
    insertLtoR basic (Node key value lhv rhv) = 
      insertLtoR (insertLtoR (insert (key,value) basic) rhv) lhv

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> Maybe (Integer, v)
nearestLE i t@(Node key value lhv rhv)
  | key == i  = findNearest i t
  | otherwise = case (nearestLE i lhv, nearestLE i rhv) of
      (Just x, _ )  -> Just x
      ( _ , Just x) -> Just x
      ( _,_ )       -> Nothing
nearestLE i _ = Nothing

findNearest i (Node key value lhv rhv) = 
  case (findNearestHelper i lhv, findNearestHelper i rhv) of
    (Just x, _ )  -> Just x
    ( _ , Just x) -> Just x
    ( _,_ )       -> Nothing
  where
    findNearestHelper i  EmptyTree            = Nothing
    findNearestHelper i (Leaf key value)      = Just (key, value)
    findNearestHelper i (Node key value _ _ ) = Just (key, value)
findNearest i  _ = Nothing

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList   []        = EmptyTree
treeFromList ((x,y):xys) = insert (x,y) $ treeFromList xys

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree  EmptyTree               = []
listFromTree (Leaf key value)         = [(key,value)]
listFromTree (Node key value lhv rhv) = (key,value) : listFromTree lhv ++ listFromTree rhv

-- Поиск k-той порядковой статистики дерева
kMean :: Int -> TreeMap v -> (Integer, v)
kMean i t = listFromTree t !! i


main :: IO ()
main = do
  putStrLn "          -- Suite 1 --"
  let t1 = EmptyTree
  let res2 = show $ contains t1 56
  putStrLn $ "contains t1 56  -> " ++ res2
  let t2 = Node 74 "some text value" (Leaf 87 "lol") (Node 3 "lurk" (Leaf 99 "red") (Leaf 98 "yolo"))
  let res2 = show $ contains t2 87
  putStrLn $ "contains t2 87  -> " ++ res2
  putStrLn "\n          -- Suit 2 --"
  let res1 = show $ lookup 98 t2
  putStrLn $ "lookup 98 t2  -> " ++ res1
  putStrLn "\n          -- Suit 3 --"
  let t3 = insert (5,"lol5") $ insert (4,"lol4") $ insert (3,"lol3") $ insert (2,"lol2") $ insert (1,"lol1") EmptyTree
  putStrLn $ "insert -> " ++ show t3
  putStrLn "\n          -- Suit 4 --"
  let t4 = remove 2 t3
  putStrLn $ "remove 2 t3 -> " ++ show t4
  putStrLn "\n          -- Suit 5 --"
  let t5 = nearestLE 2 t3
  putStrLn $ "nearestLE 2 t3 -> " ++ show t5
  putStrLn "\n          -- Suit 6 --"
  let t6 = treeFromList [(1,2),(3,4),(5,6),(7,8),(9,10)]
  putStrLn $ "treeFromList [(1,2),(3,4),(5,6),(7,8),(9,10)] -> " ++ show t6
  putStrLn "\n          -- Suit 7 --"
  let t7 = listFromTree t6
  putStrLn $ "listFromTree t6 -> " ++ show t7
  putStrLn "\n          -- Suit 8 --"
  let t8 = kMean 2 t6
  putStrLn $ "kMean 2 t6 -> " ++ show t8
  putStrLn "\n          -- Suit 9 --"
  let res9 = calcDepth t6
  putStrLn $ "calcDepth t6 -> " ++ show res9
