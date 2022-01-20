module Part3.Tasks where

import Util (notImplementedYet)
import Data.List
import Data.Map (fromList, empty, insertWith, toList)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff = iterate

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

flatDigitsList :: [Int] -> [Int]
flatDigitsList = concatMap digs

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq numList = snd (maximum [ (length ks, head ks) | ks <- group (sort (flatDigitsList numList)) ])

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (a:tail) | a `elem` tail = uniq tail
              | otherwise = a : uniq tail

data Tree a = Nil
            | Node a (Tree a) (Tree a)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k, Ord k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = Data.Map.toList (foldr ((\(mp, ms) sp -> Data.Map.insertWith (++) mp ms sp) . (\x -> (f x, [x]))) Data.Map.empty l)
