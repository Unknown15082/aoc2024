module Day3 where

import           Control.Monad
import           Data.Either
import           Data.List            (intercalate)
import           Data.Void
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char
import           Util

type Parser = Parsec Void String

intParser :: Parser Int
intParser = read <$> some digitChar

multParser :: Parser Int
multParser = do
    void $ string "mul("
    a <- intParser
    void $ string ","
    b <- intParser
    void $ string ")"
    return (a * b)

doParser :: Parser Int
doParser = do
    void $ string "do()"
    return (-1)

dontParser :: Parser Int
dontParser = do
    void $ string "don't()"
    return (-2)

programParser :: Parser [Int]
programParser = many $ try (skipManyTill anySingle (try multParser))

programParserV2 :: Parser [Int]
programParserV2 = many $ try (skipManyTill anySingle (try doParser <|> try dontParser <|> try multParser))

solveState :: Int -> [Int] -> Int
solveState _ []      = 0
solveState 0 (-1:xs) = solveState 1 xs
solveState 0 (-2:xs) = solveState 0 xs
solveState 1 (-1:xs) = solveState 1 xs
solveState 1 (-2:xs) = solveState 0 xs
solveState 1 (x:xs)  = x + solveState 1 xs
solveState 0 (_:xs)  = solveState 0 xs
solveState _ _       = error "Non-01 state"

calc :: String -> Int
calc program = sum res
    where res = fromRight [] $ parse programParser "" program

solve1 :: [String] -> Int
solve1 = calc . intercalate "@"

calc2 :: String -> Int
calc2 program = solveState 1 res
    where res = fromRight [] $ parse programParserV2 "" program

solve2 :: [String] -> Int
solve2 = calc2 . intercalate "@"

main :: IO()
main = do
    input <- getLines

    print $ solve1 input
    print $ solve2 input
