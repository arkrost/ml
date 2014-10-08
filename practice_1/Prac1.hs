{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Prac1 
  ( 
    R, Theta, X,
    GradientDescentConfig(..),
    batchGradientDescent,
    linear,
    logistic,
    diffTooSmall,
    shuffle,
    normalize,
    mean,
    norm,
    ClassificationResult(..),
    classify,
    countResult,
    fMeasurement
  ) where
import System.Random
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe

------------------------------------------------------
-- Gradient Descent
------------------------------------------------------

type R = Double
type Theta = (R, R, R)
type X = (R, R)

data GradientDescentConfig = GradientDescentConfig {
  eps :: R,
  lambda :: R,
  f :: Theta -> (X, R) -> R,
  differentiate :: Theta -> (X, R) -> Theta,
  stopCondition :: [(X, R)] -> R -> (Theta -> [(X, R)] -> R) -> Theta -> Theta -> Bool
}

batchGradientDescent :: GradientDescentConfig -> [(X, R)] -> Theta -> Theta
batchGradientDescent config dataset f0 = learn f0
  where
    m = fromIntegral $ length dataset
    alpha = -(lambda config)
    func t samples = sum $ map (\x -> f config t x / m) samples
    stop = stopCondition config dataset (eps config) func
    accTheta (t0, t1, t2) (dt0, dt1, dt2) = (t0 + dt0, t1 + dt1, t2 + dt2)
    update t dt = accTheta t (scaleTheta dt alpha)
    scaleTheta (t0, t1, t2) c = (c * t0, c * t1, c * t2)
    accDerivative f af x = accTheta af $ scaleTheta (differentiate config f x) (1 / m)
    getNext f = update f (foldl (accDerivative f) (0, 0, 0) dataset)
    learn f = let fNext = getNext f in 
        if stop f fNext then fNext else learn fNext

linear :: Theta -> X -> R
linear (t0, t1, t2) (x1, x2) = t0 + t1 * x1 + t2 * x2

logistic :: R -> R
logistic x =  1 / (1 + exp (- x))


diffTooSmall ::  [(X, R)] -> R -> (Theta -> [(X, R)] -> R) -> Theta -> Theta -> Bool
diffTooSmall samples eps f t tNext = (abs $ (f t samples) - (f tNext samples)) < eps

------------------------------------------------------
-- Normalization
------------------------------------------------------

mean :: Fractional t => [t] -> t
mean xs = sum $ map (/ c) xs
  where c = (fromIntegral $ length xs)

normalize :: (Fractional t, Ord t) => [t] ->[t]
normalize xs = let
                m = mean xs
                s = maximum xs - minimum xs in 
                  map (\x -> (x - m) / s) xs

norm :: (Fractional t, Ord t) => [((t, t), a)] -> [((t, t), a)]
norm ss = let
      (xs, ys) = unzip ss
      (x1s, x2s) = unzip xs in
        flip zip ys $ zip (normalize x1s) (normalize x2s)

------------------------------------------------------
-- f measurement
------------------------------------------------------
type I = Integer

data ClassificationResult = ClassificationResult {
  tp :: I,
  fp :: I,
  fn :: I,
  tn :: I
} deriving (Show)

dispatch :: (Bool, Bool) -> (I, I, I, I) -> (I, I, I, I)
dispatch (True, True) (tt, fp, fn , tn) = (tt + 1, fp, fn, tn)
dispatch (True, False) (tt, fp, fn , tn) = (tt, fp + 1, fn, tn)
dispatch (False, True) (tt, fp, fn , tn) = (tt, fp, fn + 1, tn)
dispatch (False, False) (tt, fp, fn , tn) = (tt, fp, fn, tn + 1)


classify :: (a -> Bool) -> [(a, Bool)] -> [(Bool, Bool)]
classify clissifier = map (\(x, y) -> (clissifier x == y, y)) 

countResult :: [(Bool, Bool)] -> ClassificationResult
countResult classified = result $ foldl (flip dispatch) (0, 0, 0, 0) classified
  where 
    result (tt, fp, fn, tn) = ClassificationResult tt fp fn tn


fMeasurement :: ClassificationResult -> Double
fMeasurement res = if (precision + recall == 0) then 0 else 2 * precision * recall / (precision + recall)
  where
    a ./. b = if (a == 0) then 0 else fromIntegral a / fromIntegral b
    precision = 0.5 * ((tp res ./. (tp res + fp res)) + (tn res ./. (tn res + fn res)))
    recall = 0.5 * ((tp res ./. (tp res + fn res)) + (tn res ./. (tn res + fp res)))

------------------------------------------------------
-- Misc
------------------------------------------------------
 
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs