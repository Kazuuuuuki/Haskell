doubleMe x y = x * 2 + y * 2
doubleSmallNumber x = if x > 100
                        then x
                        else x*2


boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]



rightTriangles' = [ (a, b, c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree  x y z = x + y + z

lucky :: Int  -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy"
head' (x:_) = x

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> Double-> String 
bmiTell weight height
  | bmi <= skinny = "You are underweight" 
  | bmi <= normal = "You're supposedly normal"
  | bmi <= fat = "You're fat! Lose some"
  | otherwise = "You're a whale, congrations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0




max' :: (Ord a) => a -> a -> a 
max' a b
     | a <= b = b
     | otherwise = a

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2 

initials :: String -> String
initials firstname = [f] ++ ".  " ++ " ."
    where (f:_) = firstname


head2 :: [a] -> a
head2 xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

descriveList :: [a] -> String
descriveList ls = "The list is "
                  ++ case ls of [] -> "empty"
                                [x] -> "a singleton list."
                                xs -> "a longer list."

  
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger


divideByTen :: (Floating a) => a -> a
divideByTen = (/10)




