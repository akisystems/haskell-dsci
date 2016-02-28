{-# LANGUAGE TemplateHaskell #-}
module CFUnitTests where 

import Test.QuickCheck
import Test.QuickCheck.All

import CF

prop_Manhattan :: Point -> Point -> Bool
prop_Manhattan a@(x1,y1) b@(x2,y2) = (manhattan a b) == (abs (x1 - x2)) + (abs  (y1 - y2))

prop_EuclidManhattanDiff :: Point -> Point -> Bool
prop_EuclidManhattanDiff a@(x1,y1) b@(x2,y2) =  someZero || notEqual
      where notEqual = ((euclidean a b) /= (manhattan a b))
            summed   = sum [x + y | (x,y) <- [a,b]]
            someZero = 0.0 == summed

return []
runTests = $quickCheckAll

