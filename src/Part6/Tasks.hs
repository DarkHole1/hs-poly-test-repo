{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map(Map, (!?), insert)
import Data.Maybe(fromMaybe)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    create :: (Int, Int) -> mx
    get :: (Int, Int) -> mx -> Int
    set :: (Int, Int) -> Int -> mx -> mx
    size :: mx -> (Int, Int)

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    create (1, 1) = 0
    size _ = (1, 1)
    get (0, 0) = id
    set (0, 0) n = const n
instance Matrix [[Int]] where
    create (w, h) = replicate h (replicate w 0)
    size x = (length (x !! 0), length x)
    get (x, y) m = m !! y !! x
    set (x, y) n = at2 y x (const n)
        where
            indexed = zip [0..]
            at x f = fmap (\(x', y) -> if x == x' then f y else y) . indexed
            at2 x y f = at x (at y f)
instance Matrix (SparseMatrix Int) where
    create (w, h) = SparseMatrix {
        sparseMatrixWidth = w, sparseMatrixHeight = h, sparseMatrixElements = mempty
    }
    size SparseMatrix { sparseMatrixWidth = w, sparseMatrixHeight = h } = (w, h)
    get p m = fromMaybe 0 $ sparseMatrixElements m !? p
    set p n m = SparseMatrix {
        sparseMatrixWidth = sparseMatrixWidth m,
        sparseMatrixHeight = sparseMatrixHeight m,
        sparseMatrixElements = e'
    }
        where
            e' = insert p n $ sparseMatrixElements m

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = foldr (\x -> set (x, x) 1) (create (w, w)) [0..w - 1]
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = create (w, h)
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b | aw == bh = c'
    where
        (aw, ah) = size a
        (bw, bh) = size b
        c = create (bw, ah)
        c' = foldr (\p -> set p (cij p)) c pts
        pts = [(x, y) | x <- [0..bw - 1], y <- [0..ah - 1]]
        cij (i, j) = sum $ fmap (\r -> get (i, r) b * get (r, j) a) [0..aw - 1]
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
