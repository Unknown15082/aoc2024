import           Data.List (inits, tails)

handleLine :: Read a => String -> [a]
handleLine = map read . words

getInts :: IO [[Int]]
getInts = map handleLine . lines <$> getContents

fAnd :: [a -> Bool] -> a -> Bool
fAnd fs x = all ($ x) fs

fOr :: [a -> Bool] -> a -> Bool
fOr fs x = any ($ x) fs

checkSmall :: [Int] -> Bool
checkSmall = all ((< 4) . abs)

checkInc :: [Int] -> Bool
checkInc = all (< 0)

checkDec :: [Int] -> Bool
checkDec = all (> 0)

safe :: [Int] -> Bool
safe = fAnd [checkSmall, fOr [checkInc, checkDec]]

check :: ([Int] -> Bool) -> [(Bool, Int, Bool)] -> Bool
check _ xs
    | length xs < 2 = False
check f (x:y:zs) = (pfx && sfx && f [a + b]) || check f (y:zs)
    where (pfx, a, _) = x
          (_, b, sfx) = y

almost :: ([Int] -> Bool) -> [Int] -> Bool
almost f x = fOr [f . tail, f . init] x || check f combined
    where pfx = map f $ inits x
          sfx = map f $ tails x
          combined = zip3 pfx x (tail sfx)

almostInc :: [Int] -> Bool
almostInc = almost (fAnd [checkSmall, checkInc])

almostDec :: [Int] -> Bool
almostDec = almost (fAnd [checkSmall, checkDec])

almostSafe :: [Int] -> Bool
almostSafe = fOr [almostInc, almostDec]

solve1 :: [[Int]] -> Int
solve1 = length . filter safe

solve2 :: [[Int]] -> Int
solve2 = length . filter (fOr [safe, almostSafe])

main :: IO()
main = do
    input <- getInts
    let diff = map (\x -> zipWith (-) x (tail x)) input
    print $ solve1 diff
    print $ solve2 diff
