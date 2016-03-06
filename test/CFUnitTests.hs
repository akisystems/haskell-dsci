{-# LANGUAGE TemplateHaskell #-}
module CFUnitTests where 

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Science.CF

prop_Manhattan :: Score -> Score -> Bool
prop_Manhattan a b = (manhattan a b) == abs (a - b)

prop_EuclidManhattanDiff :: Score -> Score -> Bool
prop_EuclidManhattanDiff a b = sumZero || notEqual
      where notEqual = ((euclidean a b) /= (manhattan a b))
            sumZero  = 0.0 == a + b
            
prop_MinkowskiMatchesManhattan :: Score -> Score -> Bool
prop_MinkowskiMatchesManhattan a b = (minkowski a b 1) == (manhattan a b)
      
prop_MinkowskiMatchesEuclidean :: Score -> Score -> Bool
prop_MinkowskiMatchesEuclidean a b = (minkowski a b 2) == (euclidean a b)

return []
runTests = $quickCheckAll