module Data.Graph.Abstract.BuilderSpec (spec) where

import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Builder as GAB
import Test.Hspec


buildSpec :: Spec
buildSpec = describe "build" $ do

    it "should create empty graph" $ do

        let g = GAB.build (pure ())

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should create graph with all verteces isolated" $ do

        let g = GAB.build $ do {
            GAB.vertex ();
            GAB.vertex ();
            pure ()
        }

        length (GA.vertices g) `shouldBe` 2
        length (GA.edges g) `shouldBe` 0

    it "should create graph with different vertex degrees" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";

            GAB.edge' b a;
            GAB.edge' c a;
            GAB.edge' c b
        }

        length (GA.vertices g) `shouldBe` 3
        length (GA.edges g) `shouldBe` 3
        GA.vertices (GA.zip g (GA.degree g)) `shouldMatchList` [("a", 0), ("b", 1), ("c", 2)]


spec :: Spec
spec = describe "Data.Graph.Abstract.Builder" $ do
    buildSpec
