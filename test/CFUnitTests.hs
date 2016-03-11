{-# LANGUAGE TemplateHaskell #-}
module CFUnitTests where 

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Science.CF

-- | An abstraction for distance functions
type DistFunc = ([Score] -> [Score] -> Score)

-- | General property for distance functions that should never yield zero/negative
atLeastZeroProp :: DistFunc -> [Score] -> [Score] -> Bool
atLeastZeroProp f a b = (f a b) >= 0

prop_ManhattanNeverZero :: [Score] -> [Score] -> Bool
prop_ManhattanNeverZero a b = atLeastZeroProp manhattan a b

prop_EuclideanNeverZero :: [Score] -> [Score] -> Bool
prop_EuclideanNeverZero a b = atLeastZeroProp euclidean a b

return []
runTests = $quickCheckAll