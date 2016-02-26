{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

import CF

prop_Manhattan :: (Float, Float) -> (Float, Float) -> Bool
prop_Manhattan a@(x1,y1) b@(x2,y2) = (manhattan a b) == (abs (x1 - x2)) + (abs  (y1 - y2))

return []
runTests = $quickCheckAll

main :: IO ()
main = runTests >> (putStrLn "Done")
