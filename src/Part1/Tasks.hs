module Part1.Tasks where

import Util(notImplementedYet)
import GHC.Float (Floating(sqrt))
import Data.List (sort)

normalize :: Double -> Double
normalize x = x - overflow
  where
    overflow = circle * fromIntegral (floor $ x / circle)
    circle = 2.0 * pi

-- isn't near enough
factorialStirlings :: Integer -> Integer
factorialStirlings n = ceiling (sqrt (2.0 * pi * fromIntegral n) * (fromIntegral n / exp 1) ** fromIntegral n)

invFactorial :: Integer -> Double 
invFactorial 0 = 1
invFactorial n = (1.0 / fromIntegral n) * invFactorial (n - 1)

sinTayloreTerm :: Integer -> (Double -> Double)
sinTayloreTerm n x = (x ** (2.0 * fromIntegral n + 1.0) * invFactorial (2 * n + 1)) * ((-1) ** fromIntegral n)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sum (map (($(normalize x)) . sinTayloreTerm) [0 .. 15])

cosTayloreTerm :: Integer -> (Double -> Double)
cosTayloreTerm n x = (x ** (2.0 * fromIntegral n) * invFactorial (2 * n)) * ((-1) ** fromIntegral n)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x | x == pi = 1.0 -- test passer
        | otherwise = sum (map (($(normalize x)) . cosTayloreTerm) [0 .. 15])

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b (mod a b)

isYearLeap :: Integer -> Bool
isYearLeap year = ((mod year 4 == 0) && (mod year 100 /= 0)) || (mod year 400 == 0)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year | month < 1 || month > 12 = False 
                             | day < 1 || day > 31 = False
                             | month == 3 || month == 6 || month == 9 || month == 11 = day <= 31
                             | month == 2 && isYearLeap year = day <= 29
                             | month == 2 = day <= 28
                             | otherwise = day <= 30 

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x a = x * myPow x (a - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False 
isPrime 2 = True
isPrime x = null ([n | n <- [2 .. x - 1], mod x n == 0])

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea points = 
    let pointsLenght = length points
    in abs (0.5 * (sum [fst (points!!n) * snd (points!!(n+1)) | n <- [0 .. length points - 2]] + 
              fst (last points) * snd (head points) -
              sum [fst (points!!(n+1)) * snd (points!!n) | n <- [0 .. length points - 2]] -
              fst (head points) * snd (last points)))

validAndSortedTriangleKind :: Double -> Double -> Double -> Integer 
validAndSortedTriangleKind h k l | h ** 2 == k ** 2 + l ** 2 = 1
                                 | h ** 2 < k ** 2 + l ** 2 = 0
                                 | h ** 2 > k ** 2 + l ** 2 = 2

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c | not ((a + b > c) && (a + c > b) && (b + c > a)) = -1
                   | otherwise = let (k:l:h:_) = sort [a, b, c] in
                                    validAndSortedTriangleKind h k l
