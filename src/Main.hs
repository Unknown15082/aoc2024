module Main where

import           Day1
import           Day2
import           Day3
import           Day4
import           Day5
import           Day6
import           Options.Applicative hiding (Parser)
import qualified Options.Applicative as Opt (Parser)

dayParser :: Opt.Parser Int
dayParser = option auto ( long "day" <> short 'd' )


main :: IO()
main = do
    day <- execParser $ info (dayParser <**> helper) fullDesc

    (case day of
        1 -> Day1.main
        2 -> Day2.main
        3 -> Day3.main
        4 -> Day4.main
        5 -> Day5.main
        6 -> Day6.main
        _ -> error "Error")
