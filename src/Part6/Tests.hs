module Part6.Tests where

import qualified Data.Map

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Part6.Tasks

unit_eye = do
    eye 1 @?= one
    eye 1 @?= [[one]]
    eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
    eye 2 @?= [[one, 0], [0, one]]
    eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])

    where one :: Int; one = 1

unit_zero = do
    zero 1 1 @?= zz
    zero 2 1 @?= [[zz, zz]]
    zero 2 2 @?= [[zz, zz], [zz, zz]]
    zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([]::[((Int, Int), Int)]))
    where zz :: Int; zz = 0

unit_multiplyMatrix = do
    multiplyMatrix a b @?= c
    where
        a :: [[Int]]
        a = [[1, 0, 1], 
            [2, 1, 1], 
            [0, 1, 1], 
            [1, 1, 2]]
        b :: [[Int]]
        b = [[1, 2, 1],
            [2, 3, 1],
            [4, 2, 2]]
        c :: [[Int]]
        c = [[5, 4, 3],
            [8, 9, 5],
            [6, 5, 3],
            [11, 9, 6]]

unit_determinant = do
    determinant (10 :: Int) @?= (10 :: Int)
    determinant ([[1, 2], [3, 4]] :: [[Int]]) @?= (-2 :: Int)
    determinant ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]]) @?= (0 :: Int)
    determinant (eye 10 :: [[Int]]) @?= 1
