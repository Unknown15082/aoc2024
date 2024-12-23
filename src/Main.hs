module Main where

import           Day1
import           Day2
import           Options.Applicative

dayParser :: Parser Int
dayParser = option auto ( long "day" <> short 'd' )

main :: IO()
main = do
    day <- execParser $ info (dayParser <**> helper) fullDesc

    (case day of
        1 -> Day1.main
        2 -> Day2.main
        _ -> error "Error")
