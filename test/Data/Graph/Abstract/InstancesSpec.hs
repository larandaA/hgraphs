module Data.Graph.Abstract.InstancesSpec (spec) where

import qualified Data.Maybe as M
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Common as GAC
import Data.Graph.Abstract.Instances
import Test.Hspec


fmapSpec :: Spec
fmapSpec = describe "fmap" $ do

    it "should preserve empty graph" $ do

        let g = fmap (+ 1) GAC.empty

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should not change graph with id function" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge "e1" b a;
            GA.edge "e2" c a;
            GA.edge "e3" c b
        }
        let g' = fmap id g

        GA.vertices g' `shouldMatchList` ["a", "b", "c"]
        GA.edges g' `shouldMatchList` [("b", "e1", "a"), ("c", "e2", "a"), ("c", "e3", "b")]

    it "should not change graph structure" $ do

        let g = GA.build $ do {
            v0 <- GA.vertex 0;
            v1 <- GA.vertex 1;
            v2 <- GA.vertex 2;

            GA.edge "e1" v1 v0;
            GA.edge "e2" v2 v0;
            GA.edge "e3" v2 v1
        }
        let g' = fmap (+ 1) g

        GA.vertices g' `shouldMatchList` [1, 2, 3]
        GA.edges g' `shouldMatchList` [(2, "e1", 1), (3, "e2", 1), (3, "e3", 2)]

foldrSpec :: Spec
foldrSpec = describe "foldr" $ do

    it "should return a default value on an empty graph" $ do

        let g = GAC.empty

        foldr (+) 42 g `shouldBe` 42

    it "should return a number of vertices with sum function" $ do

        let g = GAC.isolated [1, 1, 1, 1, 1]

        foldr (+) 0 g `shouldBe` 5

traverseSpec :: Spec
traverseSpec = describe "traverse" $ do

    it "should return a pure graph if traversed with pure function" $ do

        let g = GAC.isolated [1, 2, 3]
        let mg = traverse pure g

        M.isJust mg `shouldBe` True
        
        let (Just g') = mg

        GA.vertices g' `shouldMatchList` [1, 2, 3]
        length (GA.edges g') `shouldBe` 0

    it "should return nothing if one of the vertices is mapped to nothing" $ do

        let g = GAC.isolated [1, 2, 3]

        let f 2 = Nothing
            f _ = Just ()
        
        M.isNothing (traverse f g) `shouldBe` True


spec :: Spec
spec = describe "Data.Graph.Abstract.Instances" $ do
    fmapSpec
    foldrSpec
    traverseSpec
