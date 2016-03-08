module Data.Science.CF
where

import qualified Data.Map.Strict as M

type Score = Float

type ScoreMap b = (M.Map b Score)

-- | The collection of a user's ratings
-- | where a is the user identifier type, b is the item identifier type
data UserSamples a b = Rating a (ScoreMap b) deriving (Show, Eq)

-- | The minkowski distance function, where r is the exponent
minkowski :: Score -> Score -> Int -> Score
minkowski a b r = dif ** ex
  where dif = (abs (a - b)) :: Float
        ex  = (fromIntegral r)

-- | The manhattan distance function
manhattan :: Score -> Score -> Score
manhattan a b = minkowski a b 1

-- | The euclidean distance function
euclidean :: [Score] -> [Score] -> Score
euclidean l r = sqrt $ foldl mink 0 $ zip l r
  where mink u (a,b) = u + minkowski a b 2

-- | Using the the function supplied, compute the distance from the two sets of scores
-- | for ratings that are eligible
distanceWith :: (Ord k) => (Score -> Score -> Score) -> ScoreMap k -> ScoreMap k -> Score
distanceWith f a b = 
  M.foldrWithKey (\k v t -> 
      case (M.lookup k b) of
        Just v2 -> t + (f v v2)
        Nothing -> t) 0 a

-- | Apply the given distance function across all eligable ratings
-- | Eligible ratings are the intersection of l and r
distancesWith :: (Ord k) => ([Score] -> [Score] -> Score) -> ScoreMap k -> ScoreMap k -> Score
distancesWith f l r = f (M.elems l) (M.elems r)
  where isect = M.intersection l r