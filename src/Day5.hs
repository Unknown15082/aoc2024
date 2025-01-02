module Day5 where

import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Util

getMiddle :: [Int] -> Int
getMiddle xs = xs !! (length xs `div` 2)

checks :: [Int] -> [[Int]] -> Bool
checks = all . check
    where check page [x, y] = isNothing iX || isNothing iY || fromJust iX < fromJust iY
              where iX = elemIndex x page
                    iY = elemIndex y page
          check _ _ = False

fix :: [[Int]] -> [Int] -> [Int]
fix _ [] = []
fix rules page = nxt : fix rules removed
    where removed = filter (/= nxt) page
          nxt = fromJust $ find valid page
          valid x = all (check x) rules
          check x [a, b]
              | b == x = a `notElem` page
              | otherwise = True
          check _ _      = True

solve1 :: [[Int]] -> [[Int]] -> Int
solve1 rules pages = sum (map getMiddle correct)
    where correct = filter (`checks` rules) pages

solve2 :: [[Int]] -> [[Int]] -> Int
solve2 rules pages = sum $ map getMiddle fixed
    where fixed = map (fix rules) (filter (not . (`checks` rules)) pages)

main :: IO()
main = do
    input <- getLines
    let (strRules, strPages) = break (== "") input
    let rules = map (map read . splitOn "|") strRules :: [[Int]]
    let pages = map (map read . splitOn ",") (tail strPages) :: [[Int]]

    print $ solve1 rules pages
    print $ solve2 rules pages
