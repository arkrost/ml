module Main where
import Control.Applicative ((<$>))
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Prac1

h :: Theta -> X -> R
h t x = logistic $ linear t x

cost :: Theta -> (X, R) -> R
cost t (x, y) = -y * (log $ h t x) - (1 - y) * (log $ 1 - (h t x))

dCost :: Theta -> (X, R) -> Theta
dCost t (x@(x1, x2), y) = let c = (h t x - y) in (c, c * x1, c * x2)


filename = "chips.txt"
splitFactor = 0.8
config = GradientDescentConfig {
  eps = 1e-6,
  lambda = 1e-3,
  f = cost,
  differentiate = dCost,
  stopCondition = diffTooSmall
}

main :: IO ()
main = do 
  samples <- (parse <$> readFile filename) :: IO [(X, R)]
  dataset <- norm <$> return samples
  let 
    splitIndx = floor $ splitFactor *. length dataset
    traindata = take splitIndx dataset 
    predictor = batchGradientDescent config traindata (1, 1, 1)
    classifier = (== 1) . round . (h predictor)
    (trainset, testset) = splitAt splitIndx $ map (\(x, y) -> (x, 1 == y)) dataset
    testAvg = percents . countResult $ classify classifier testset
    trainAvg = percents . countResult $ classify classifier trainset
    in do
      print predictor
      printf "Average mistake on testset is %.2f%%\n" (testAvg :: Double)
      printf "Average mistake on trainset is %.2f%%\n" (trainAvg :: Double)
  where
    parse = map (parseEntry . splitOn ",") . lines
    parseEntry [x1, x2, y] = ((read x1, read x2), read y)
    x ./. y = fromIntegral x / fromIntegral y
    x *. y = x * fromIntegral y
    percents res = 100.0 * (fp res + fn res) ./. (tp res + tn res + fp res + fn res)