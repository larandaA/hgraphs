module Data.GraphSpec (spec) where

import Control.Monad.ST
import qualified Data.Graph as G
import Test.Hspec


buildFromListSpec :: Spec
buildFromListSpec = describe "buildFromList" $ do

    it "should create empty graph" $ do

        let g = G.buildFromList [] []

        (G.numVerteces g) `shouldBe` 0
        (G.numEdges g) `shouldBe` 0

    it "should create graph with all verteces isolated" $ do

        let g = G.buildFromList [(), ()] []

        (G.numVerteces g) `shouldBe` 2
        (G.numEdges g) `shouldBe` 0

    it "should create graph with different vertex degrees" $ do

        let g = G.buildFromList [(), (), ()] [G.Edge 0 1 (), G.Edge 0 2 (), G.Edge 1 2 ()]

        (G.numVerteces g) `shouldBe` 3
        (G.numEdges g) `shouldBe` 3
        (G.deg 0 g) `shouldBe` 0
        (G.deg 1 g) `shouldBe` 1
        (G.deg 2 g) `shouldBe` 2

spec :: Spec
spec = describe "Data.Graph" $ do
    buildFromListSpec
