{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Day6 where

import           Data.Array
import           Data.List
import           Data.Maybe
import           Util

type Int2 = (Int, Int)
type String2D = Array Int2 Char

mkString2D :: [String] -> String2D
mkString2D strs = array ((0, 0), (n-1, m-1)) [((i, j), strs !! i !! j) | i <- [0..n-1], j <- [0..m-1]]
    where n = length strs
          m = length $ head strs

getSize :: String2D -> (Int, Int)
getSize str = (n, m)
    where ((l1, l2), (u1, u2)) = bounds str
          n = u1 - l1 + 1
          m = u2 - l2 + 1

addTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addTuple (a, b) (c, d) = (a + c, b + d)

getDir :: Int -> Int2
getDir x = [(-1, 0), (0, 1), (1, 0), (0, -1)] !! x

rotateRight :: Int -> Int
rotateRight dir = (dir + 1) `mod` 4

findStart :: String2D -> Int2
findStart grid = fromJust $ find (\idx -> (grid ! idx) == '^') (indices grid)

countInArray :: Eq b => b -> Array a b -> Int
countInArray value = length . filter (== value) . elems

gridTrace :: String2D -> Int2 -> Int -> String2D
gridTrace grid idx dir = if nxtPosInside then newGrid else grid // [(idx, 'X')]
    where movement = getDir dir
          nxtPos = addTuple idx movement
          nxtPosInside = inRange (bounds grid) nxtPos
          nxtPosEmpty = (grid ! nxtPos) /= '#'
          newGridForward = gridTrace (grid // [(idx, 'X')]) nxtPos dir
          newGridTurn = gridTrace grid idx (rotateRight dir)
          newGrid = if nxtPosEmpty then newGridForward else newGridTurn

gridCheck :: String2D -> Int2 -> Int -> Int -> Bool
gridCheck grid idx dir path
    | path > 8 * n * m = True
    | otherwise = nxtPosInside && newGrid
    where (n, m) = getSize grid
          movement = getDir dir
          nxtPos = addTuple idx movement
          nxtPosInside = inRange (bounds grid) nxtPos
          nxtPosEmpty = (grid ! nxtPos) /= '#'
          newGridForward = gridCheck grid nxtPos dir (path + 1)
          newGridTurn = gridCheck grid idx (rotateRight dir) (path + 1)
          newGrid = if nxtPosEmpty then newGridForward else newGridTurn

solve1 :: String2D -> Int
solve1 grid = countInArray 'X' (gridTrace (grid // [(start, 'X')]) start 0)
    where start = findStart grid

solve2 :: String2D -> Int
solve2 grid = length $ filter (\pos -> gridCheck (grid // [(pos, '#')]) start 0 0) posList
    where start = findStart grid
          posList = filter (\idx -> (idx /= start) && (grid ! idx /= '#')) (indices grid)

main :: IO()
main = do
    input <- getLines
    let grid = mkString2D input

    print $ solve1 grid
    print $ solve2 grid
