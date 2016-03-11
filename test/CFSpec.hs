module CFSpec where 
import qualified Data.Map.Strict as M
import Data.Science.CF
import Test.Hspec

angelica = M.fromList [("Blues Traveler", 3.5), 
                        ("Broken Bells", 2.0),
                        ("Norah Jones", 4.5),
                        ("Phoenix", 5.0),
                        ("Slightly Stoopid", 1.5),
                        ("The Strokes", 2.5),
                        ("Vampire Weekend", 2.0)]
                        
bill = M.fromList [("Blues Traveler", 2.0),
                   ("Broken Bells", 3.5),
                   ("Deadmau5", 4.0),
                   ("Phoenix", 2.0),
                   ("Slightly Stoopid", 3.5),
                   ("Vampire Weekend", 3.0)]

hailey = M.fromList [("Broken Bells", 4.0),
                     ("Deadmau5", 1.0),
                     ("Norah Jones", 4.0),
                     ("The Strokes", 4.0),
                     ("Vampire Weekend", 1.0)]

jordyn = M.fromList [("Broken Bells", 4.5),
                     ("Deadmau5", 4.0),
                     ("Norah Jones", 5.0),
                     ("Phoenix", 5.0),
                     ("Slightly Stoopid", 4.5),
                     ("The Strokes", 4.0),
                     ("Vampire Weekend", 4.0)]

veronica = M.fromList [("Blues Traveler", 3.0),
                       ("Norah Jones", 5.0),
                       ("Phoenix", 4.0),
                       ("Slightly Stoopid", 2.5),
                       ("The Strokes", 3.0)]

--chan = M.fromList [("Blues Traveler", 5.0), "Broken Bells": 1.0,
-- "Deadmau5": 1.0, "Norah Jones": 3.0,
-- "Phoenix": 5, "Slightly Stoopid": 1.0},

-- "Dan": {"Blues Traveler": 3.0, "Broken Bells": 4.0,
-- "Deadmau5": 4.5, "Phoenix": 3.0,
-- "Slightly Stoopid": 4.5, "The Strokes": 4.0,
-- "Vampire Weekend": 2.0},

-- "Sam": {"Blues Traveler": 5.0, "Broken Bells": 2.0,
-- "Norah Jones": 3.0, "Phoenix": 5.0,
-- "Slightly Stoopid": 4.0, "The Strokes": 5.0},

emptySamples :: M.Map String a
emptySamples = M.empty

specMain :: IO ()
specMain = hspec $ do
    describe "CF" $ do
        it "distanceWith manhattan should correctly compute" $ do
            (distanceWith manhattan hailey veronica) `shouldBe` 2.0
            (distanceWith manhattan hailey jordyn) `shouldBe` 7.5
            (distanceWith manhattan angelica bill) `shouldBe` 9.0
            
        it "distanceWith manhattan should result in 0 for no ratings in common" $ do
            (distanceWith manhattan emptySamples emptySamples) `shouldBe` 0.0
            (distanceWith manhattan angelica emptySamples) `shouldBe` 0.0
            (distanceWith manhattan emptySamples bill) `shouldBe` 0.0
            (distanceWith manhattan angelica (M.difference angelica bill)) `shouldBe` 0.0
            
        it "euclidean should correctly compute" $ do
            (distanceWith euclidean angelica bill) `shouldBe` 4.3011627
            (distanceWith euclidean hailey veronica) `shouldBe` (sqrt 2)

        it "euclidean should result in 0 for no ratings in common" $ do
            (distanceWith euclidean emptySamples emptySamples) `shouldBe` 0.0
            (distanceWith euclidean angelica M.empty) `shouldBe` 0.0
            (distanceWith euclidean emptySamples bill) `shouldBe` 0.0
            -- Need to think about 0.0 when no difference, as this leads to misreading nothing in common,
            -- e.g. (distanceWith euclidean bill bill) == 0.0, but they've everything in common
            
        --it "sortNeighbour for Hailey" $ do
          --(sortNeighbours euclidean hailey []) `shouldBe` [(2.0, "Veronica"), (4.0, "Chan"),(4.0, "Sam"), (4.5, "Dan"), (5.0, "Angelica"), (5.5, "Bill"), (7.5, "Jordyn")]