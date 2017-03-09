import Data.List
import Control.Monad
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x 
                                _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((y * x):ys)
foldingFunction (x:y:ys) "+" = return ((y + x):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result


--canReachIn :: Int -> KnightPos -> KnightPos -> Bool
--canReachIn x start end = end `elem` inMany x start
 
--imMany :: Int -> KnightPos -> [KnightPos]
--inMany x start = return start >>= foldr (<=<)
--  return (replicate x moveKnight)