import Control.Monad
import Data.Char

--main = forever $ do 
--  l <- getLine
--  putStrLn $ map toUpper l

--main = do
--  contents <- getContents
--  putStr $ map toUpper contents

main = do
  contents <- getContents
  putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines