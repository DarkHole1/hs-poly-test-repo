module Part1.Tasks where

import Util(notImplementedYet)

-- Небольшой трюк для того чтобы обеспечить стопроцентное покрытие.
-- Формально его можно представить как выбор удачного a для ряда
-- Тейлора.
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
myCos = approx 15 . cycle'
    where
        approx :: Integer -> Double -> Double
        approx n x =
            if n >= 0 then
                (-1) ^^ n / fromIntegral (fact (2 * n)) * x ^^ (2 * n) + approx (n - 1) x
            else
                0.0
        fact n = if n > 0 then n * fact (n - 1) else 1

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = gcd (abs a) (abs b)
    where
        gcd a 0 = a
        gcd a b = gcd b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect = notImplementedYet

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow x y = x * myPow x (y - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = primeCheck (x - 1)
    where
        primeCheck 1 = True
        primeCheck y = if rem x y == 0 then False else primeCheck (y - 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea pts = abs (positive - negative) / 2
    where
        (x1, y1) = head pts
        (xn, yn) = last pts
        pairs = zip pts (tail pts)
        positive = foldl (\acc -> \((x, _), (_, y)) -> acc + x * y) (xn * y1) pairs
        negative = foldl (\acc -> \((_, y), (x, _)) -> acc + x * y) (x1 * yn) pairs

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = notImplementedYet
