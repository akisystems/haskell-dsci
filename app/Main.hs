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
-- cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell =
    -- do char '"'
    do content <- many
    --   char '"' <?> "quote at end of cell"
       return content

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

-- | Convert the CSV records to our Samples; ugly & not proud of this!
csvToSamples :: [[String]] -> [Sample String String]
csvToSamples samples = M.foldlWithKey toRating [] $ csvToMap
    where toTuple [u,i,r,_] = (u, (i, read r :: Float)) -- extract only the uid, item id and rating
          tuples = map toTuple samples
          theFold m (u, ratingTupe) = M.insertWith (++) u [ratingTupe] m
          csvToMap = foldl theFold M.empty tuples -- Fold the tuples into a map of uid->[(itemId, rating)]
          toRating x k v = (Rating k (M.fromList v)) : x

parseCSV :: String -> String -> Either ParseError [[String]]
parseCSV iden input = parse csvFile ("(" ++ iden ++ ")") input

doSuggest :: String -> String -> IO ()
doSuggest file user = do
    contents <- readFile file
    case (parseCSV file contents) of
        Left  e -> putStrLn $ "CSV parse error: " ++ (show e)
        Right s -> let samples = map csvToSamples s
                   in putStrLn $ show samples
        -- Right s  -> let samples    = map csvToSamples s
        --                 userSample = findUser user samples
        --                 recs       = recommend euclidean userSample samples
        --                 printRec (item, sc) = putStrLn ("Item: " ++ item ++ ", Score: " ++ (show sc))
        --             in mapM_ printRec recs

main :: IO ()
main = do
    opts <- getRecord "dsci"
    case opts of
        (Suggest f u) -> doSuggest f u
        _ -> putStrLn "What?"
