module Data.Graph.Abstract.Accessor.Algorithm.DfsSpec (spec) where

import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
import qualified Data.Graph.Abstract.Accessor.Algorithm.Dfs as Dfs
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import Test.Hspec


preorderFromSpec :: Spec
preorderFromSpec = describe "preorderFrom" $ do

    it "should return default values for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            values <- Dfs.preorderFrom [] 1 (\_ _ -> pure 42);
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [1, 1, 1, 1]

    it "should return default values for unreachable vertices" $ do

        let g = GAC.isolated [1, 3, 2, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (< 3);
            values <- Dfs.preorderFrom vs "N" (\_ _ -> pure "Y");
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, "Y"), (2, "Y"), (3, "N"), (4, "N")]

    it "should return distances from start vertex" $ do

        let g = GAC.path ["a", "b", "c", "d"]

        let f Nothing _ = pure 0
            f (Just (dist, _)) _ = pure (dist + 1)

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== "a");
            values <- Dfs.preorderFrom vs (-1) f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 0), ("b", 1), ("c", 2), ("d", 3)]

    it "should return incremented values of verices" $ do

        let g = GAC.path [1, 2, 3, 4]

        let f _ v = (+ 1) <$> GAA.value v

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            values <- Dfs.preorderFrom vs (-1) f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, 2), (2, 3), (3, 4), (4, 5)]

preorderSpec :: Spec
preorderSpec = describe "preorder" $ do

    it "should work fine with empty graph" $ do

        let g = GAC.empty

        let g' = GAA.execute g $ do {
            values <- Dfs.preorder 1 (\_ _ -> pure 42);
            GAA.vgraph values
        }

        length (GA.vertices g') `shouldBe` 0

    it "should return non-default values for all vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (< 3);
            values <- Dfs.preorder 1 (\_ _ -> pure 42);
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [42, 42, 42, 42]

postorderFromSpec :: Spec
postorderFromSpec = describe "postorderFrom" $ do

    it "should return default values for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            values <- Dfs.postorderFrom [] 1 (\_ _ _ -> pure 42);
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [1, 1, 1, 1]

    it "should return default values for unreachable vertices" $ do

        let g = GAC.isolated [1, 3, 2, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (< 3);
            values <- Dfs.postorderFrom vs "N" (\_ _ _ -> pure "Y");
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, "Y"), (2, "Y"), (3, "N"), (4, "N")]

    it "should return number of direct edges" $ do

        let g = GAC.path ["a", "b", "c", "d"]

        let f _ es _ = pure (length es)

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== "a");
            values <- Dfs.postorderFrom vs (-1) f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 1), ("b", 1), ("c", 1), ("d", 0)]

    it "should return back edges description" $ do

        let g = GAC.cycle ["a", "b", "c", "d"]

        let f _ _ [] = pure "No"
            f _ _ [(_, Just _)] = pure "E1"
            f _ _ [(_, Nothing)] = pure "E2"
            f _ _ _ = pure "Else"

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== "a");
            values <- Dfs.postorderFrom vs "" f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", "No"), ("b", "No"), ("c", "No"), ("d", "E2")]

    it "should return incremented values of verices" $ do

        let g = GAC.path [1, 2, 3, 4]

        let f v _ _ = (+ 1) <$> GAA.value v

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            values <- Dfs.postorderFrom vs (-1) f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, 2), (2, 3), (3, 4), (4, 5)]

postorderSpec :: Spec
postorderSpec = describe "postorder" $ do

    it "should work fine with empty graph" $ do

        let g = GAC.empty

        let g' = GAA.execute g $ do {
            values <- Dfs.postorder 1 (\_ _ _ -> pure 42);
            GAA.vgraph values
        }

        length (GA.vertices g') `shouldBe` 0

    it "should return non-default values for all vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (< 3);
            values <- Dfs.postorder 1 (\_ _ _ -> pure 42);
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [42, 42, 42, 42]


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor.Algorithm.Dfs" $ do
    preorderFromSpec
    preorderSpec
    postorderFromSpec
    postorderSpec
