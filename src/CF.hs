module CF
-- rename to haskdsci
    ( Sample,
      manhattan
--, --      minkowski
    ) where

type Item = String

type Score = Float

type Point = (Score, Score)

data Sample a = Rating a [(Item, Score)] deriving (Show, Eq)

manhattan :: Point -> Point -> Score
manhattan a@(x1,y1) b@(x2,y2) = (abs (x1 - x2)) + (abs  (y1 - y2))

--minkowski 
