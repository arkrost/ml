{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where
import Control.Applicative ((<$>))
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Prac1

sse :: Theta -> (X, R) -> R
sse t (x, y) = 0.5 * (linear t x  - y) ^ 2

dSSE :: Theta -> (X, R) -> Theta
dSSE t (x@(x1, x2), y) = let c = (linear t x - y) in (c, c * x1, c * x2)
      
filename = "prices.txt"
splitFactor = 0.8
config = GradientDescentConfig {
  eps = 1e-12,
  lambda = 0.01,
  f = sse,
  differentiate = dSSE,
  stopCondition = diffTooSmall
}

main :: IO ()
main = do 
  samples <- (parse <$> readFile filename) :: IO [(X, R)]
  dataset <- norm <$> return samples
  let 
    splitIndx = floor $ splitFactor *. length dataset
    (trainset, testset) = splitAt splitIndx dataset 
    predictor = batchGradientDescent config trainset (1, 1, 1)
    testAvg = avg $ percents (linear predictor) testset
    trainAvg = avg $ percents (linear predictor) trainset
    in do
      print predictor
      printf "Average mistake on testset is %.2f%%\n" testAvg
      printf "Average mistake on trainset is %.2f%%\n" trainAvg
  where
    parse = map (parseEntry . splitOn ",") . lines
    parseEntry [x1, x2, y] = ((read x1, read x2), read y)
    x /. y = x / fromIntegral y
    x *. y = x * fromIntegral y
    percents f datas = map (\(x, y) -> 100.0 * (abs (f x - y)) / y) datas
    avg xs = sum xs /. length xs