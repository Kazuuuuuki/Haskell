import Data.List
import Data.Char
import qualified Data.Map as Map


numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String 
decode shift msg = encode (negate shift) msg

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v): xs)
  | key == k = Just v
  | otherwise = findKey key xs
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phpneBookToMap xs = Map.fromListWith add xs
  where add number1 number2 = number1 ++ ", " ++ number2


