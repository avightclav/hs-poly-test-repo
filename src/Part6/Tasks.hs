{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       makeEye :: Int -> mx
       makeZero :: Int -> Int -> mx
       size :: mx -> (Int, Int)
       store :: (Int, Int) -> Int -> mx -> mx
       storeAll:: [(Int, Int)] -> Int -> mx -> mx
       storeAll [] _ mx = mx
       storeAll (first:tail) value matrix = storeAll tail value (store first value matrix)

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       makeEye p = 1
       makeZero 1 1 = 0
       makeZero _ _ = error "Size Out Of Range"
       size _ = (1, 1)
       store (0, 0) value mx = value
       store _ _ _ = error "Index Out Of Bounds"
instance Matrix [[Int]] where
       makeEye p = [ [ f i j | i <- [1..p] ] | j <- [1..p] ]
                   where f i j = if i == j then 1 else 0
       makeZero w h = [ [ 0 | i <- [1..w] ] | j <- [1..h] ]
instance Matrix (SparseMatrix Int) where
       makeEye p = SparseMatrix p p ( Prelude.foldr (uncurry Data.Map.insert) Data.Map.empty [((j, j), 1)| j <- [0..p-1]])
       makeZero w h = SparseMatrix w h Data.Map.empty
       size mx = (sparseMatrixWidth mx, sparseMatrixHeight mx)

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = makeEye
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = makeZero 
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = notImplementedYet
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
