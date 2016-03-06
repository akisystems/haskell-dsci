module CFSpec (
    specMain
  ) where
  
import qualified Data.Map.Strict as M
import Data.Science.CF
import Test.Hspec

-- musicDataset :: [Sample String String]
samplesForAngelica = M.fromList [("Blues Traveler", 3.5 :: Float), 
                        ("Broken Bells", 2.0),
                        ("Norah Jones", 4.5),
                        ("Phoenix", 5.0),
                        ("Slightly Stoopid", 1.5),
                        ("The Strokes", 2.5),
                        ("Vampire Weeekend", 2.0)]
                        
samplesForHailey = M.fromList [("Broken Bells", 4.0),
                                 ("Deadmau5", 1.0),
                                 ("Norah Jones", 4.0),
                                 ("The Strokes", 4.0),
                                 ("Vampire Weeekend", 1.0)]

samplesForVeronica = M.fromList [("Blues Traveler", 3.0),
                                  ("Norah Jones", 5.0),
                                  ("Phoenix", 4.0),
                                  ("Slightly Stoopid", 2.5),
                                  ("The Strokes", 3.0)]
                        
-- "Bill": {"Blues Traveler": 2.0, "Broken Bells": 3.5,
-- "Deadmau5": 4.0, "Phoenix": 2.0,
-- "Slightly Stoopid": 3.5, "Vampire Weekend": 3.0},

-- "Chan": {"Blues Traveler": 5.0, "Broken Bells": 1.0,
-- "Deadmau5": 1.0, "Norah Jones": 3.0,
-- "Phoenix": 5, "Slightly Stoopid": 1.0},

-- "Dan": {"Blues Traveler": 3.0, "Broken Bells": 4.0,
-- "Deadmau5": 4.5, "Phoenix": 3.0,
-- "Slightly Stoopid": 4.5, "The Strokes": 4.0,
-- "Vampire Weekend": 2.0},


-- "Jordyn": {"Broken Bells": 4.5, "Deadmau5": 4.0, "Norah Jones": 5.0,
-- "Phoenix": 5.0, "Slightly Stoopid": 4.5,
-- "The Strokes": 4.0, "Vampire Weekend": 4.0},
-- "Sam": {"Blues Traveler": 5.0, "Broken Bells": 2.0,
-- "Norah Jones": 3.0, "Phoenix": 5.0,
-- "Slightly Stoopid": 4.0, "The Strokes": 5.0},

specMain :: IO ()
specMain = hspec $ do
    describe "CF" $ do
        it "distance should compute the distance between two sample sets" $ do
            (distance manhattan samplesForHailey samplesForVeronica) `shouldBe` 2.0