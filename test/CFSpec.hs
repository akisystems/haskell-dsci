module CFSpec where 
import qualified Data.Map.Strict as M
import Data.Science.CF
import Test.Hspec

angelica = Rating "Angelica" $ M.fromList [("Blues Traveler", 3.5), 
                                           ("Broken Bells", 2.0),
                                           ("Norah Jones", 4.5),
                                           ("Phoenix", 5.0),
                                           ("Slightly Stoopid", 1.5),
                                           ("The Strokes", 2.5),
                                           ("Vampire Weekend", 2.0)]
                        
bill = Rating "Bill" $ M.fromList [("Blues Traveler", 2.0),
                                   ("Broken Bells", 3.5),
                                   ("Deadmau5", 4.0),
                                   ("Phoenix", 2.0),
                                   ("Slightly Stoopid", 3.5),
                                   ("Vampire Weekend", 3.0)]
                   
chan = Rating "Chan" $ M.fromList [("Blues Traveler", 5.0),
                                   ("Broken Bells", 1.0),
                                   ("Deadmau5", 1.0),
                                   ("Norah Jones", 3.0),
                                   ("Phoenix", 5),
                                   ("Slightly Stoopid", 1.0)]

dan = Rating "Dan" $ M.fromList [("Blues Traveler", 3.0),
                                 ("Broken Bells", 4.0),
                                 ("Deadmau5", 4.5),
                                 ("Phoenix", 3.0),
                                 ("Slightly Stoopid", 4.5),
                                 ("The Strokes", 4.0),
                                 ("Vampire Weekend", 2.0)]


hailey = Rating "Hailey" $ M.fromList [("Broken Bells", 4.0),
                                       ("Deadmau5", 1.0),
                                       ("Norah Jones", 4.0),
                                       ("The Strokes", 4.0),
                                       ("Vampire Weekend", 1.0)]

jordyn = Rating "Jordyn" $ M.fromList [("Broken Bells", 4.5),
                                       ("Deadmau5", 4.0),
                                       ("Norah Jones", 5.0),
                                       ("Phoenix", 5.0),
                                       ("Slightly Stoopid", 4.5),
                                       ("The Strokes", 4.0),
                                       ("Vampire Weekend", 4.0)]

sam = Rating "Sam" $ M.fromList [("Blues Traveler", 5.0),
                                 ("Broken Bells", 2.0),
                                 ("Norah Jones", 3.0),
                                 ("Phoenix", 5.0),
                                 ("Slightly Stoopid", 4.0),
                                 ("The Strokes", 5.0)]

veronica = Rating "Veronica" $ M.fromList [("Blues Traveler", 3.0),
                                           ("Norah Jones", 5.0),
                                           ("Phoenix", 4.0),
                                           ("Slightly Stoopid", 2.5),
                                           ("The Strokes", 3.0)]
                       
allSamples = [angelica, bill, chan, dan, hailey, jordyn, sam, veronica]

emptySamples = Rating "Empty" M.empty

nothingInCommon :: Ord b => UserSamples String b -> UserSamples String b -> UserSamples String b
nothingInCommon u1 u2 = Rating "Diff" diff
  where diff = M.difference (ratings u1) (ratings u2)

specMain :: IO ()
specMain = hspec $ do
    describe "CF" $ do
        it "manhattan should correctly compute" $ do
            (computeDistance manhattan hailey veronica) `shouldBe` 2.0
            (computeDistance manhattan hailey jordyn) `shouldBe` 7.5
            (computeDistance manhattan angelica bill) `shouldBe` 9.0
            
        it "manhattan should result in 0 for no ratings in common" $ do
            (computeDistance manhattan emptySamples emptySamples) `shouldBe` 0.0
            (computeDistance manhattan angelica emptySamples) `shouldBe` 0.0
            (computeDistance manhattan emptySamples bill) `shouldBe` 0.0
            (computeDistance manhattan angelica (nothingInCommon angelica bill)) `shouldBe` 0.0
            
        it "euclidean should correctly compute" $ do
            (computeDistance euclidean angelica bill) `shouldBe` 4.3011627
            (computeDistance euclidean hailey veronica) `shouldBe` (sqrt 2)

        it "euclidean should result in 0 for no ratings in common" $ do
            (computeDistance euclidean emptySamples emptySamples) `shouldBe` 0.0
            (computeDistance euclidean angelica emptySamples) `shouldBe` 0.0
            (computeDistance euclidean emptySamples bill) `shouldBe` 0.0
            -- Need to think about 0.0 when no difference, as this leads to misreading nothing in common,
            -- e.g. (computeDistance euclidean bill bill) == 0.0, but they've everything in common
            
        --it "sortNeighbour for Hailey" $ do
          --(sortNeighbours euclidean hailey allSamples) `shouldBe` [(2.0, "Veronica"), (4.0, "Chan"),(4.0, "Sam"), (4.5, "Dan"), (5.0, "Angelica"), (5.5, "Bill"), (7.5, "Jordyn")]