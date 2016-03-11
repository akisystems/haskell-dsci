module Data.Science.CF
where

import qualified Data.Map.Strict as M
import Data.List

type Score = Float

type ScoreMap b = (M.Map b Score)

-- | The collection of a user's ratings
-- | where a is the user identifier type, b is the item identifier type
data UserSamples a b = Rating a (ScoreMap b) deriving (Show, Eq)

-- | Return just the item ratings for the given sample
ratings :: UserSamples a b -> ScoreMap b
ratings (Rating a m) = m

-- | The minkowski distance function, where r is the exponent
minkowski :: Score-> Score -> Int -> Score
minkowski a b r = dif ** ex
  where dif = (abs (a - b)) :: Float
        ex  = (fromIntegral r)

-- | The manhattan distance function
manhattan :: [Score] -> [Score] -> Score
manhattan [] _ = 0
manhattan _ [] = 0
manhattan l r = foldl mink 0 $ zip l r
  where mink u (a,b) = u + minkowski a b 1

-- | The euclidean distance function
euclidean :: [Score] -> [Score] -> Score
euclidean l r = sqrt $ foldl mink 0 $ zip l r
  where mink u (a,b) = u + minkowski a b 2

-- | Apply the given distance function across all eligable ratings
-- | Eligible ratings are the intersection of l and r
distanceWith :: (Ord k) => ([Score] -> [Score] -> Score) -> ScoreMap k -> ScoreMap k -> Score
distanceWith f l r = f lItems rItems
  where lIsect = M.intersection l r
        rIsect = M.intersection r l
        lItems = (M.elems lIsect)
        rItems = (M.elems rIsect)
        
computeDistance :: (Ord k) => ([Score] -> [Score] -> Score) -> UserSamples k k -> UserSamples k k -> Score
computeDistance f l r = distanceWith f lr rr
  where lr = ratings l
        rr = ratings r
        
-- | Compute the distances for the given samples from the user, using the function supplied, then sort by closest
sortNeighbours :: (Ord k) => ([Score] -> [Score] -> Score) -> UserSamples k k -> [UserSamples k k] -> [(Score, k)]
sortNeighbours f u s = sort $ map dist s
  where iden (Rating a m) = a
        dist sample = (computeDistance f u sample, iden sample)