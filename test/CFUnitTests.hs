{-# LANGUAGE TemplateHaskell #-}
module CFUnitTests where 

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Science.CF

prop_Manhattan :: Score -> Score -> Bool
prop_Manhattan a b = (manhattan a b) == abs (a - b)

prop_EuclideanNeverZero :: [Score] -> [Score] -> Bool
prop_EuclideanNeverZero a b = sumZero || equal || (euclidean a b) >= 0
      where equal = a == b
            sumA       = sum a
            sumB       = sum b
            sumZero  = 0.0 == sumA + sumB
            
prop_MinkowskiMatchesManhattan :: Score -> Score -> Bool
prop_MinkowskiMatchesManhattan a b = (minkowski a b 1) == (manhattan a b)

return []
runTests = $quickCheckAll