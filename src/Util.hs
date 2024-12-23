module Util where

handleLine :: Read a => String -> [a]
handleLine = map read . words

getInts :: IO [[Int]]
getInts = map handleLine . lines <$> getContents

getLines :: IO [String]
getLines = lines <$> getContents
