{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map
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
    create (w, h) = replicate w (replicate h 0)
    size x = (length x, length (x !! 0))
    get (x, y) m = m !! x !! y
    set (x, y) n = at2 x y (const n)
        where
            indexed = zip [0..]
            at x f = fmap (\(x', y) -> if x == x' then f y else y) . indexed
            at2 x y f = at x (at y f)
instance Matrix (SparseMatrix Int) where
    create (w, h) = SparseMatrix {
        sparseMatrixWidth = w, sparseMatrixHeight = h, sparseMatrixElements = fromList []
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
eye w = notImplementedYet
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = notImplementedYet
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = notImplementedYet
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
