{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Science.CF
import qualified Data.Map.Strict as M
import Options.Generic
import Text.ParserCombinators.Parsec

class ParseCSV o where
    getCSV :: o -> IO [[String]]
    
instance ParseCSV Opt where
    getCSV (Suggest c _) = (readFile c) >>= (parseIt . parseCSV)
    getCSV (Closest c _) = (readFile c) >>= (parseIt . parseCSV)

csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n\r")

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

data Opt = Suggest { csv :: String, user :: String } -- the recommendation
         | Closest { csv :: String, user :: String } 
    deriving (Generic, Show)

instance ParseRecord Opt

insUserRating :: M.Map String [(String, Score)] -> [String] -> M.Map String [(String, Score)]
insUserRating m (u:i:r:_) = M.insertWith (++) u [(i, read r :: Float)] m -- insert a tuple at key u, into m, "squashing" repeat rows in the CSV
insUserRating m _ = error "Invalid CSV structure - give me 3 columns"

-- | Convert the CSV records to our Samples
csvToSamples :: [[String]] -> [Sample String String]
csvToSamples samples = M.foldlWithKey toRating [] $ csvToMap
    where csvToMap = foldl insUserRating M.empty samples -- Fold the tuples into a map of uid->[(itemId, rating)]
          toRating x k v = (Rating k (M.fromList v)) : x

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "CSV parse" input

makeRecs :: String -> [[String]] -> [(String, Score)]
makeRecs user csv = recommend euclidean userSample samples
  where samples    = csvToSamples csv
        userSample = findUser user samples
        
closest :: String -> [[String]] -> [(Score, String)]
closest = closestNeighbour euclidean userSample samples
    where samples = csvToSamples csv
          userSample = findUser user samples

doSuggest :: String -> String -> IO ()
doSuggest file user = do
    contents <- readFile file
    case (parseCSV contents) of
        Left  e -> putStrLn $ "CSV parse error: " ++ (show e)
        Right s -> putStrLn $ show $ makeRecs user s

parseIt :: (Either ParseError [[String]]) -> IO [[String]]
parseIt (Left e) = error $ show e
parseIt (Right c) = return c


main :: IO ()
main = do
    opts <- getRecord "dsci"
    csv <- getCSV opts
    case opts of
        Suggest _ u -> putStrLn $ show $ makeRecs u csv
        Closest _ u -> putStrLn $ show $ closest u csv
