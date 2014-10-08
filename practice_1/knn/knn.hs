{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where
import Data.List (sortBy)
import Control.Applicative ((<$>))
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn)
import Text.Printf (printf)
import System.IO.Unsafe

import Prac1 (shuffle, norm, classify, countResult, fMeasurement, ClassificationResult(..))

------------------------------------------------------------------------
-- knn algorithm
------------------------------------------------------------------------
type Distance = Double
class FeatureVector fv where
  dist :: fv -> fv -> Distance

type ClassLabel = Bool

knn :: (FeatureVector fv, Show fv) => [(fv, ClassLabel)] -> Int -> fv -> ClassLabel
knn dataset k fv = getClass kNearest
	where
		kNearest = take k $ sortBy distComparator dataset
		distComparator (v1, _) (v2, _) = compare (dist v1 fv) (dist v2 fv)
		getClass nearest = k `div` 2 < (length $ filter snd nearest)

type Point2D = (Double, Double)
instance FeatureVector Point2D where
  dist v1 v2 = euclid v1 v2
    where 
    	euclid (x1,y1) (x2,y2) = sqrt (square (x1 - x2) + square (y1 - y2))
    	square x = x * x
------------------------------------------------------------------------
-- main
------------------------------------------------------------------------

filename = "chips.txt"
splitFactor = 0.8
trainSplit = 0.75

main :: IO ()
main = do 
	samples <- (norm <$> parse <$> readFile filename) :: IO [(Point2D, Bool)]
	dataset <- shuffle samples
	let 
		splitIndx = round $ splitFactor *. length dataset
		(trainset, testset) = splitAt splitIndx dataset 
		trainSplitIndx = round $ trainSplit *. length trainset
		(trainingset, validationset) = splitAt trainSplitIndx trainset
		classifier = knn trainingset
		mbK = [1 .. min 10 ((length validationset) `div` 2)]
		measure = fMeasurement . countResult . (flip classify validationset) . classifier
		classified = zip mbK $ map measure mbK
		best = maximumBy (comparing snd) classified
		bestClassifier = knn trainset $ fst best
		checkres = countResult $ classify bestClassifier testset
		percent = (100.0 * (tp checkres + tn checkres) /. length testset) :: Double 
		in do
			printf "k = %d\n" (fst best)
			printf "Classified succesfully %d of %d(%.2f%%)\n"  
				(tp checkres + tn checkres) (length testset) percent
	where
		parse = map (parseEntry . splitOn ",") . lines
		parseEntry [x,y,cl] = ((read x, read y), 1 == read cl)
		x /. y = fromIntegral x / fromIntegral y
		x *. y = x * fromIntegral y
