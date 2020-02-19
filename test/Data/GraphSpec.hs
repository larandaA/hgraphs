module Data.GraphSpec (spec) where

import Control.Monad.ST
import qualified Data.Graph as G
import Test.Hspec


verticesSpec :: Spec
verticesSpec = describe "vertices" $ do

    it "should be empty for empty graph" $ do

        let g = G.build (pure ())

        G.vertices g `shouldBe` ([] :: [()])

    it "should return all vertices for graph without edges" $ do

        let g = G.build $ do {
            G.vertex "a";
            G.vertex "b";
            pure ()
        }

        G.vertices g `shouldMatchList` ["a", "b"]

    it "should return vertices with repeated labels" $ do

        let g = G.build $ do {
            G.vertex "a";
            G.vertex "b";
            G.vertex "a";
            pure ()
        }

        G.vertices g `shouldMatchList` ["a", "a", "b"]

    it "should return all vertices for graph with edges" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            G.edge a b ()
        }

        G.vertices g `shouldMatchList` ["a", "b"]

edgesSpec :: Spec
edgesSpec = describe "edges" $ do

    it "should be empty for empty graph" $ do

        let g = G.build (pure ())

        G.edges g `shouldBe` ([] :: [((), (), ())])

    it "should be empty for graph without edges" $ do

        let g = G.build $ do {
            G.vertex "a";
            G.vertex "b";
            pure ()
        }

        G.edges g `shouldBe` ([] :: [(String, (), String)])

    it "should return all edges for graph with edges" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            c <- G.vertex "c";
            G.edge a b ();
            G.edge a c ();
            G.edge b c ()
        }

        G.edges g `shouldMatchList` [("a", (), "b"), ("a", (), "c"), ("b", (), "c")]

buildSpec :: Spec
buildSpec = describe "build" $ do

    it "should create empty graph" $ do

        let g = G.build (pure ())

        length (G.vertices g) `shouldBe` 0
        length (G.edges g) `shouldBe` 0

    it "should create graph with all verteces isolated" $ do

        let g = G.build $ do {
            G.vertex ();
            G.vertex ();
            pure ()
        }

        length (G.vertices g) `shouldBe` 2
        length (G.edges g) `shouldBe` 0

    it "should create graph with different vertex degrees" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            c <- G.vertex "c";

            G.edge b a ();
            G.edge c a ();
            G.edge c b ()
        }

        length (G.vertices g) `shouldBe` 3
        length (G.edges g) `shouldBe` 3
        G.vertices (G.degree g) `shouldMatchList` [("a", 0), ("b", 1), ("c", 2)]


spec :: Spec
spec = describe "Data.Graph" $ do
    buildSpec
    verticesSpec
    edgesSpec
