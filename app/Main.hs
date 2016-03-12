{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Science.CF
import Options.Generic

data Opts = Suggest { dataFile :: String, user :: String} -- the recommendation
          | Blah    { bob :: String }
    deriving (Generic, Show)

instance ParseRecord Opts

toSample :: String -> Sample String String
toSample line = error "lol"

doSuggest :: String -> String -> IO ()
doSuggest file user = do
    contents <- readFile file
    let lines' = lines contents
    let samples = map toSample lines'
    let userSample = findUser user samples
    let recs = recommend euclidean userSample samples
    mapM_ printRec recs
    where printRec (item, sc) = putStrLn ("Item: " ++ item ++ ", Score: " ++ (show sc))

main :: IO ()
main = do
    opts <- getRecord "dsci"
    case opts of
        (Suggest f u) -> doSuggest f u
        _ -> putStrLn "What?"
