import           Data.Function (on)
import           Data.List

handleLine :: Read a => String -> [a]
handleLine = map read . words

getInts :: IO [[Int]]
getInts = map handleLine . lines <$> getContents

solve1 :: [[Int]] -> Int
solve1 x = let
    [a, b] = map sort $ transpose x
    in sum $ map abs $ zipWith (-) a b

compress :: [Int] -> [(Int, Int)]
compress = map (\x -> (head x, length x)) . group

solve2 :: [[Int]] -> Int
solve2 x = calc a b
    where [a, b] = map (compress . sort) $ transpose x
          combined = sortBy (compare `on` fst) (a ++ b)
          value [x]    = 0
          value [x, y] = uncurry (*) x * snd y
          calc a b = sum . map value $ groupBy ((==) `on` fst) combined

main :: IO()
main = do
    input <- getInts
    print $ solve1 input
    print $ solve2 input
