module Day4 where

import           Data.Array
import           Data.Bifunctor
import           Util

type Int2 = (Int, Int)
type String2D = Array Int2 Char

getSize :: String2D -> (Int, Int)
getSize str = (n, m)
    where ((l1, l2), (u1, u2)) = bounds str
          n = u1 - l1 + 1
          m = u2 - l2 + 1

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

checkXMAS :: String2D -> Int2 -> Int
checkXMAS str dir = length (filter check possible)
    where b = bounds str
          moveAlong = map (($ dir) . mapTuple) [(*0), (*1), (*2), (*3)]
          applyOnIdx idx (x, y) = (fst idx + x, snd idx + y)
          possible = filter (\idx -> inRange b (applyOnIdx idx $ mapTuple (*3) dir)) (indices str)
          check idx = map (str !) idxs == "XMAS"
              where idxs = map (applyOnIdx idx) moveAlong

checkCrossMAS :: String2D -> Int2 -> Bool
checkCrossMAS str (i, j) = str ! (i, j) == 'A' && any (("MMSS" ==) . map (str !)) cyc_idxs
    where idxs = [(i-1, j-1), (i-1, j+1), (i+1, j+1), (i+1, j-1)]
          cyc_idxs = take 4 $ iterate cyc idxs
              where cyc x = last x:init x

solve1 :: String2D -> Int
solve1 str = sum $ map (checkXMAS str) dirs
    where dirs = [(i, j) | i <- [-1..1], j <- [-1..1]]

solve2 :: String2D -> Int
solve2 str = length $ filter (checkCrossMAS str) idxs
    where b = bounds str
          idxs = range (bimap (mapTuple (+1)) (mapTuple (subtract 1)) b)

main :: IO()
main = do
    input <- getLines
    let n = length input
    let m = length $ head input

    let str = array ((0, 0), (n-1, m-1)) [((i, j), input !! i !! j) | i <- [0..n-1], j <- [0..m-1]]

    print $ solve1 str
    print $ solve2 str
