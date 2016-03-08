module CFSpec (
    specMain
  ) where
  
import qualified Data.Map.Strict as M
import Data.Science.CF
import Test.Hspec

samplesForAngelica = M.fromList [("Blues Traveler", 3.5), 
                        ("Broken Bells", 2.0),
                        ("Norah Jones", 4.5),
                        ("Phoenix", 5.0),
                        ("Slightly Stoopid", 1.5),
                        ("The Strokes", 2.5),
                        ("Vampire Weekend", 2.0)]
                        
samplesForBill = M.fromList [("Blues Traveler", 2.0),
                             ("Broken Bells", 3.5),
                             ("Deadmau5", 4.0),
                             ("Phoenix", 2.0),
                             ("Slightly Stoopid", 3.5),
                             ("Vampire Weekend", 3.0)]
                             
                        
samplesForHailey = M.fromList [("Broken Bells", 4.0),
                                 ("Deadmau5", 1.0),
                                 ("Norah Jones", 4.0),
                                 ("The Strokes", 4.0),
                                 ("Vampire Weekend", 1.0)]
                                 
samplesForJordyn = M.fromList [("Broken Bells", 4.5),
                               ("Deadmau5", 4.0),
                               ("Norah Jones", 5.0),
                               ("Phoenix", 5.0),
                               ("Slightly Stoopid", 4.5),
                               ("The Strokes", 4.0),
                               ("Vampire Weekend", 4.0)]


samplesForVeronica = M.fromList [("Blues Traveler", 3.0),
                                  ("Norah Jones", 5.0),
                                  ("Phoenix", 4.0),
                                  ("Slightly Stoopid", 2.5),
                                  ("The Strokes", 3.0)]

emptySamples :: M.Map String a
emptySamples = M.empty

-- "Chan": {"Blues Traveler": 5.0, "Broken Bells": 1.0,
-- "Deadmau5": 1.0, "Norah Jones": 3.0,
-- "Phoenix": 5, "Slightly Stoopid": 1.0},

-- "Dan": {"Blues Traveler": 3.0, "Broken Bells": 4.0,
-- "Deadmau5": 4.5, "Phoenix": 3.0,
-- "Slightly Stoopid": 4.5, "The Strokes": 4.0,
-- "Vampire Weekend": 2.0},

-- "Sam": {"Blues Traveler": 5.0, "Broken Bells": 2.0,
-- "Norah Jones": 3.0, "Phoenix": 5.0,
-- "Slightly Stoopid": 4.0, "The Strokes": 5.0},

specMain :: IO ()
specMain = hspec $ do
    describe "CF" $ do
        it "distanceWith manhattan should correctly compute" $ do
            (distanceWith manhattan samplesForHailey samplesForVeronica) `shouldBe` 2.0
            (distanceWith manhattan samplesForHailey samplesForJordyn) `shouldBe` 7.5
            (distanceWith manhattan samplesForAngelica samplesForBill) `shouldBe` 9.0
            
        it "distanceWith manhattan should result in 0 for no ratings in common" $ do
            (distanceWith manhattan emptySamples emptySamples) `shouldBe` 0.0
            (distanceWith manhattan samplesForAngelica emptySamples) `shouldBe` 0.0
            (distanceWith manhattan emptySamples samplesForBill) `shouldBe` 0.0
            (distanceWith manhattan samplesForAngelica (M.difference samplesForAngelica samplesForBill)) `shouldBe` 0.0
            
        it "euclidean should correctly compute" $ do
            (distancesWith euclidean samplesForAngelica samplesForBill) `shouldBe` 4.2426405

        it "euclidean should result in 0 for no ratings in common" $ do
            (distancesWith euclidean emptySamples emptySamples) `shouldBe` 0.0
            (distancesWith euclidean samplesForAngelica M.empty) `shouldBe` 0.0
            (distancesWith euclidean emptySamples samplesForBill) `shouldBe` 0.0
            -- Need to think about 0.0 when no difference, as this leads to misreading nothing in common,
            -- e.g. (distanceWith euclidean samplesForBill samplesForBill) == 0.0, but they've everything in common