module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ c [] = c
myFoldl f c (x:xs) = myFoldl f (f c x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ c [] = c
myFoldr f c (x:xs) = f x (myFoldr f c xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = notImplementedYet

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = notImplementedYet

myConcat :: [[a]] -> [a]
myConcat = notImplementedYet

myReverse :: [a] -> [a]
myReverse = notImplementedYet

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = notImplementedYet

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = notImplementedYet

