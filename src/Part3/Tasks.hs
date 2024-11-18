module Part3.Tasks where

import Util (notImplementedYet)
import Data.List(elemIndex)
import Data.Maybe(fromJust)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq xs = maxIndex $ map (flip count digits) ['0'..'9']
    where
        digits = concatMap show xs
        count x = length . filter (x==)
        maxIndex xs = fromJust $ elemIndex (maximum xs) xs

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = uniq' []
    where
        uniq' xs (y:ys)
            | y `elem` xs = uniq' xs ys
            | otherwise = uniq' (y:xs) ys
        uniq' xs _ = xs

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = foldl (\a -> \b -> appendRes (f b) b a) [] l
    where
        appendRes :: (Eq k) => k -> a -> [(k, [a])] -> [(k, [a])]
        appendRes k a [] = [(k, [a])]
        appendRes k a ((k', as):ks)
            | k == k' = (k, a:as):ks
            | otherwise = (k', as) : appendRes k a ks
