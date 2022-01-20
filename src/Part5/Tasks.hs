module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl func acc [] = acc
myFoldl func acc (head:tail) = myFoldl func (func acc head) tail

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr func acc [] = acc
myFoldr func acc (head:tail) = func head (myFoldr func acc tail)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap func = myFoldr (\elem acc -> func elem:acc) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap func = myFoldr (\elem acc -> func elem ++ acc) []

myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\elem acc -> if p elem then elem : acc else acc) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\elem (accTrue, accFalse) -> if p elem then (elem : accTrue, accFalse) else (accTrue, elem : accFalse)) ([], [])

