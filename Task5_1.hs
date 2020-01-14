module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


-- Реализуйте функции индексирования, вставки и удаления элементов
index :: DList a -> Int -> a
index  DNil            _ = error "Index out of range"
index (DCons _ h _   ) 0 = h
index (DCons _ h DNil) _ = error "Index out of range"
index (DCons _ h t   ) i = index t (i-1)


insertAt :: DList a -> Int -> a -> DList a
insertAt  DNil         0 v = DCons DNil v DNil
insertAt  DNil         _ v = error "Index out of range"
insertAt (DCons l h r) 0 v = x where
  x = DCons l v y
  y = DCons x h r
insertAt (DCons l h r) i v = DCons l h (insertAt r (i-1) v)


removeAt :: DList a -> Int -> DList a
removeAt  DNil            _ = DNil
removeAt (DCons DNil h r) 0 = r
removeAt (DCons (DCons ll lh _) _ (DCons _ rh rr)) 0 = x where
  x = DCons ll lh y
  y = DCons x  rh rr
removeAt (DCons l h r)    i = DCons l h (removeAt r (i-1))


main :: IO ()
main = do
  let dlist = list2dlist [1,2,3,4,5,6,7,8]
  let res1  = index dlist 4
  putStrLn $ "index 4          -> " ++ show res1
  let res2  = insertAt dlist 5 9
  putStrLn $ "insertAt 5 9     -> " ++ show res2
  let res3  = removeAt dlist 5
  putStrLn $ "removeAt dlist 5 -> " ++ show res3