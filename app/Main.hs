{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Science.CF
import qualified Data.Map.Strict as M
import Options.Generic
import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol
line = sepBy cell (char '\t')
cell = many (noneOf "\t\n\r")

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

data Opts = Suggest { csv :: String, user :: String } -- the recommendation
          | Blah    { bob :: String }
    deriving (Generic, Show)

instance ParseRecord Opts

-- | Convert the CSV records to our Samples
csvToSamples :: [[String]] -> [Sample String String]
csvToSamples samples = M.foldlWithKey toRating [] $ csvToMap
    where insUserRating m [u,i,r,_] = M.insertWith (++) u [(i, read r :: Float)] m -- insert a tuple at key u, into m, "squashing" repeat rows in the CSV
          csvToMap = foldl insUserRating M.empty samples -- Fold the tuples into a map of uid->[(itemId, rating)]
          toRating x k v = (Rating k (M.fromList v)) : x

parseCSV :: String -> String -> Either ParseError [[String]]
parseCSV iden input = parse csvFile ("(" ++ iden ++ ")") input

makeRecs :: String -> [[String]] -> [(String, Score)]
makeRecs user csv = recommend euclidean userSample samples
  where samples    = csvToSamples csv
        userSample = findUser user samples

doSuggest :: String -> String -> IO ()
doSuggest file user = do
    contents <- readFile file
    case (parseCSV file contents) of
        Left  e -> putStrLn $ "CSV parse error: " ++ (show e)
        Right s -> putStrLn $ show $ makeRecs user s

main :: IO ()
main = do
    opts <- getRecord "dsci"
    case opts of
        Suggest csvFile userId -> doSuggest csvFile userId
        _ -> putStrLn "What?"
