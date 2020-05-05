module Data.Graph.Abstract.Accessor.Algorithm.BfsSpec (spec) where

import Control.Monad
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
import qualified Data.Graph.Abstract.Accessor.Algorithm.Bfs as Bfs
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import Test.Hspec


bfsFromSpec :: Spec
bfsFromSpec = describe "bfsFrom" $ do

    it "should return default values for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            values <- Bfs.bfsFrom [] 1 (\_ _ -> pure 42);
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [1, 1, 1, 1]

    it "should return default values for unreachable vertices" $ do

        let g = GAC.isolated [1, 3, 2, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (< 3);
            values <- Bfs.bfsFrom vs "N" (\_ _ -> pure "Y");
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, "Y"), (2, "Y"), (3, "N"), (4, "N")]

    it "should return distances from start vertices" $ do

        let g = GAC.path ["s1", "b", "s2", "d", "e"]

        let f Nothing _ = pure 0
            f (Just (dist, _)) _ = pure (dist + 1)

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind ((>= 2) . length);
            values <- Bfs.bfsFrom vs (-1) f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [("s1", 0), ("b", 1), ("s2", 0), ("d", 1), ("e", 2)]

    it "should return incremented values of vertices" $ do

        let g = GAC.path [1, 2, 3, 4]

        let f _ v = (+ 1) <$> GAA.value v

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== 1);
            values <- Bfs.bfsFrom vs (-1) f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, 2), (2, 3), (3, 4), (4, 5)]

bfsSpec :: Spec
bfsSpec = describe "bfs" $ do

    it "should work fine with empty graph" $ do

        let g = GAC.empty

        let g' = GAA.execute g $ do {
            values <- Bfs.bfs 1 (\_ _ -> pure 42);
            GAA.vgraph values
        }

        length (GA.vertices g') `shouldBe` 0

    it "should return non-default values for all vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            values <- Bfs.bfs 1 (\_ _ -> pure 42);
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [42, 42, 42, 42]


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor.Algorithm.Bfs" $ do
    bfsFromSpec
    bfsSpec
