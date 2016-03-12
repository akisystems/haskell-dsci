{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Science.CF
import Options.Generic

data Opts = Suggest { dataFile :: String, user :: String} -- the recommendation
          | Blah    { bob :: String }
    deriving (Generic, Show)
    
instance ParseRecord Opts

doSuggest :: String -> String -> IO ()
doSuggest file user = do
    contents <- readFile file
    let lines' = lines contents
    mapM_ putStrLn lines'

main :: IO ()
main = do
    opts <- getRecord "dsci"
    case opts of
        (Suggest f u) -> doSuggest f u
        _ -> putStrLn "What?"
