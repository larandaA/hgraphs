module Data.Graph.Abstract.InstancesSpec (spec) where

import qualified Data.Maybe as M
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Common as GAC
import Data.Graph.Abstract.Instances
import Test.Hspec


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
    foldrSpec
    traverseSpec
