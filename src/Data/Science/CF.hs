module Data.Science.CF
where

import qualified Data.Map.Strict as M

type Score = Float

type Point = (Score, Score)

type Samples b = [(b, Score)]

type ScoreMap b = (M.Map b Score)

-- | The collection of a user's ratings
-- | where a is the user identifier type, b is the item identifier type
data UserSamples a b = Rating a (ScoreMap b) deriving (Show, Eq)

-- | The minkowski distance function, where r is the exponent
minkowski :: Score -> Score -> Int -> Score
minkowski a b r = (abs (a - b)) ^ r

-- | The manhattan distance function
manhattan :: Score -> Score -> Score
manhattan a b = minkowski a b 1

-- | The euclidean distance function
euclidean :: Score -> Score -> Score
euclidean a b = minkowski a b 2

distance :: (Score -> Score -> Score) -> ScoreMap String -> ScoreMap String -> Score
distance f a b = sum $ map snd $ M.toList $ 
  M.mapWithKey (\k v -> 
    case (M.lookup k b) of
      Just v2 -> f v v2
      _      -> 0) a