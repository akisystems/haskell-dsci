{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

import CFUnitTests
import CFSpec

main :: IO ()
main = runTests >> specMain
