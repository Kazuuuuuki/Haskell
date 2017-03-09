import Control.Monad.Instances

addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a+b)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen2) = random newGen
      (thirdCoin, newGen3) = random newGen2
  in (firstCoin, secondCoin, thirdCoin)

  