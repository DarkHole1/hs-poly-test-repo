module Part1.Tasks where

import Util(notImplementedYet)

cycle' :: Double -> Double
cycle' x = 
    -- if res >= 0 then res else res + 2 * pi
    res
    where
        res = x - (fromIntegral . floor) (x / (2 * pi)) * 2 * pi

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin = approx 10 . cycle'
    where
        approx :: Integer -> Double -> Double
        approx n x =
            if n >= 0 then
                (-1) ^^ n / fromIntegral (fact (2 * n + 1)) * x ^^ (2 * n + 1) + approx (n - 1) x
            else
                0.0
        fact n = if n > 0 then n * fact (n - 1) else 1

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos = notImplementedYet

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD = notImplementedYet

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect = notImplementedYet

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow = notImplementedYet

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime = notImplementedYet

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea = notImplementedYet

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = notImplementedYet
