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
contains (Node key _ lhv rhv) k
  | key == k  = True
  | k > key   = contains rhv k
  | otherwise = contains lhv k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> Maybe v
lookup k  EmptyTree = Nothing
lookup k (Leaf key value )
  | key == k  = Just value
  | otherwise = Nothing
lookup k (Node key value lhv rhv)
  | key == k  = Just value
  | k > key   = lookup k rhv
  | otherwise = lookup k lhv

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k,v)  EmptyTree        = Leaf k v
insert (k,v) (Leaf key value)
  | k > key   = Node key value EmptyTree (Leaf k v)
  | otherwise = Node key value (Leaf k v) EmptyTree
insert (k,v) (Node key value lhv rhv)
  | k > key   = Node key value lhv (insert (k,v) rhv)
  | otherwise = Node key value (insert (k,v) lhv) rhv

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i EmptyTree = EmptyTree
remove i x@(Leaf key value)
  | key == i  = EmptyTree
  | otherwise = x
remove i (Node key value lhv rhv)
  | key == i  = case liftRightElement lhv of
    (Just (newKey, newValue), newLhv) -> Node newKey newValue newLhv rhv
    (Nothing,                 newLhv) -> EmptyTree
  | i > key   = Node key value  lhv (remove i rhv)
  | otherwise = Node key value (remove i lhv) rhv
  where
    liftRightElement EmptyTree                = (Nothing,          EmptyTree)
    liftRightElement l@(Leaf key value)       = (Just (key,value), EmptyTree)
    liftRightElement (Node key value lhv rhv) = (deletedValue,     Node key value lhv newRightTree)
      where
        (deletedValue, newRightTree) = liftRightElement rhv

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> Maybe (Integer, v)
nearestLE i t@(Node key value lhv rhv)
  | key == i  = findNearest t
  | i > key   = nearestLE i rhv
  | otherwise = nearestLE i lhv
nearestLE i _ = Nothing

findNearest (Node key value lhv rhv) = 
  case (findNearestHelper lhv, findNearestHelper rhv) of
    (Just x, _ )  -> Just x
    ( _ , Just x) -> Just x
    ( _,_ )       -> Nothing
  where
    findNearestHelper  EmptyTree            = Nothing
    findNearestHelper (Leaf key value)      = Just (key, value)
    findNearestHelper (Node key value _ _ ) = Just (key, value)
findNearest _ = Nothing

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList = foldl (\tree (x,y) -> insert (x,y) tree) EmptyTree

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
  let tree1 = Node 74 "some text value" (Leaf 3 "lurk") (Node 98 "yolo" (Leaf 87 "lol") (Leaf 99 "red"))
  putStrLn $ "tree1  -> " ++ show tree1
  let res2 = show $ contains tree1 56
  putStrLn $ "\ncontains tree1 56  -> " ++ res2
  let res2 = show $ contains tree1 87
  putStrLn $ "contains tree1 87  -> " ++ res2
  putStrLn "\n          -- Suit 2 --"
  let res1 = show $ lookup 98 tree1
  putStrLn $ "lookup 98 tree1  -> " ++ res1
  putStrLn "\n          -- Suit 3 --"
  let tree3 = insert (7,"lol7") $ insert (1,"lol1") $ insert (3,"lol3") $ insert (2,"lol2") $ insert (6,"lol6") EmptyTree
  putStrLn $ "insert -> " ++ show tree3
  putStrLn "\n          -- Suit 4 --"
  let t4 = remove 6 tree3
  putStrLn $ "remove 6 tree3 -> " ++ show t4
  putStrLn "\n          -- Suit 5 --"
  let t5 = nearestLE 6 tree3
  putStrLn $ "nearestLE 6 tree3 -> " ++ show t5
  putStrLn "\n          -- Suit 6 --"
  let t6 = treeFromList [(7,8),(5,6),(1,2),(3,4),(9,10)]
  putStrLn $ "treeFromList [(7,8),(5,6),(1,2),(3,4),(9,10)] -> " ++ show t6
  putStrLn "\n          -- Suit 7 --"
  let t7 = listFromTree t6
  putStrLn $ "listFromTree t6 -> " ++ show t7
  putStrLn "\n          -- Suit 8 --"
  let t8 = kMean 2 t6
  putStrLn $ "kMean 2 t6 -> " ++ show t8
