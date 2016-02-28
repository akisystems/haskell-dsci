{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

import CFUnitTests

main :: IO ()
main = runTests >> (putStrLn "Done")
