module CF
-- rename to haskdsci
    ( Point,
      Sample,
      manhattan,
      euclidean
--, --      minkowski
    ) where

type Item = String

type Score = Float

type Point = (Score, Score)

data Sample a = Rating a [(Item, Score)] deriving (Show, Eq)

-- | The manhattan distance function
manhattan :: Point -> Point -> Score
manhattan (x1,y1) (x2,y2) = (abs (x1 - x2)) + (abs  (y1 - y2))

-- | The euclidean distance function
euclidean :: Point -> Point -> Score
euclidean (x1,y1) (x2,y2) = sqrt $ xsq + ysq
  where x = (x1 - x2)
        xsq = x * x
        y = (y1 - y2)
        ysq = y * y
