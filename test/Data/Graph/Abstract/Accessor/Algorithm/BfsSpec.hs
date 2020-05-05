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

distancesSpec :: Spec
distancesSpec = describe "distances" $ do

    it "should be Nothing for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            distances <- Bfs.distances [];
            GAA.vgraph distances
        }

        GA.vertices g' `shouldMatchList` [Nothing, Nothing, Nothing, Nothing]

    it "should be 0 for all vertices if all vertices are start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            distances <- Bfs.distances vs;
            GAA.vgraph distances
        }

        GA.vertices g' `shouldMatchList` [Just 0, Just 0, Just 0, Just 0]

    it "should be 0 for a single vertex on isolated vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== 3);
            distances <- Bfs.distances vs;
            GAA.vgraph distances
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (1, Nothing), (2, Nothing)
            , (3, Just 0), (4, Nothing)
            ]

    it "should be 1 for rays of a star" $ do

        let g = GAC.star 4 [1, 2, 3]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== 4);
            distances <- Bfs.distances vs;
            GAA.vgraph distances
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (1, Just 1), (2, Just 1)
            , (3, Just 1), (4, Just 0)
            ]

    it "should be the distances of the shortest paths" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;
            v4 <- GAB.vertex 4;

            GAB.edge' v0 v1;
            GAB.edge' v1 v2;
            GAB.edge' v2 v4;
            GAB.edge' v0 v3;
            GAB.edge' v3 v4
        }

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== 0);
            distances <- Bfs.distances vs;
            GAA.vgraph distances
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (0, Just 0), (1, Just 1)
            , (2, Just 2), (3, Just 1)
            , (4, Just 2)
            ]

pathsSpec :: Spec
pathsSpec = describe "paths" $ do

    it "should be Nothing for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            preds <- Bfs.paths [];
            preds' <- GAA.varray Nothing;
            forM_ vs $ \v -> do {
                pred <- GAA.vget preds v;
                pred' <- traverse (GAA.value . GAA.source) pred;
                GAA.vset preds' v pred';
            };
            GAA.vgraph preds'
        }

        GA.vertices g' `shouldMatchList` [Nothing, Nothing, Nothing, Nothing]

    it "should be Nothing for all vertices if all vertices are start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            preds <- Bfs.paths vs;
            preds' <- GAA.varray Nothing;
            forM_ vs $ \v -> do {
                pred <- GAA.vget preds v;
                pred' <- traverse (GAA.value . GAA.source) pred;
                GAA.vset preds' v pred';
            };
            GAA.vgraph preds'
        }

        GA.vertices g' `shouldMatchList` [Nothing, Nothing, Nothing, Nothing]

    it "should be star center for rays of a star" $ do

        let g = GAC.star 4 [1, 2, 3]

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            starts <- GAA.vfind (== 4);
            preds <- Bfs.paths starts;
            preds' <- GAA.varray Nothing;
            forM_ vs $ \v -> do {
                pred <- GAA.vget preds v;
                pred' <- traverse (GAA.value . GAA.source) pred;
                GAA.vset preds' v pred';
            };
            GAA.vgraph preds'
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (1, Just 4), (2, Just 4)
            , (3, Just 4), (4, Nothing)
            ]

    it "should be the predecessors according to the shortest paths" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;
            v4 <- GAB.vertex 4;

            GAB.edge' v0 v1;
            GAB.edge' v1 v2;
            GAB.edge' v2 v4;
            GAB.edge' v0 v3;
            GAB.edge' v3 v4
        }

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            starts <- GAA.vfind (== 0);
            preds <- Bfs.paths starts;
            preds' <- GAA.varray Nothing;
            forM_ vs $ \v -> do {
                pred <- GAA.vget preds v;
                pred' <- traverse (GAA.value . GAA.source) pred;
                GAA.vset preds' v pred';
            };
            GAA.vgraph preds'
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (0, Nothing), (1, Just 0)
            , (2, Just 1), (3, Just 0)
            , (4, Just 3)
            ]


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor.Algorithm.Bfs" $ do
    bfsFromSpec
    bfsSpec
    distancesSpec
    pathsSpec
