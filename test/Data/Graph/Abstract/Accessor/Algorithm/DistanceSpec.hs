module Data.Graph.Abstract.Accessor.Algorithm.DistanceSpec (spec) where

import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
import qualified Data.Graph.Abstract.Accessor.Algorithm.Distance as Dist
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import Data.Maybe (isJust, isNothing, fromJust)
import Test.Hspec


dijkstraSpec :: Spec
dijkstraSpec = describe "dijkstra" $ do

    it "should be Nothing for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            distances <- Dist.dijkstra [] (const 1);
            GAA.vgraph distances
        }

        GA.vertices g' `shouldMatchList` [Nothing, Nothing, Nothing, Nothing]

    it "should be 0 for all vertices if all vertices are start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            distances <- Dist.dijkstra vs (const 1);
            GAA.vgraph distances
        }

        GA.vertices g' `shouldMatchList` [Just 0, Just 0, Just 0, Just 0]

    it "should be 0 for a single vertex on isolated vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== 3);
            distances <- Dist.dijkstra vs (const 1);
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
            distances <- Dist.dijkstra vs (const 1);
            GAA.vgraph distances
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (1, Just 1), (2, Just 1)
            , (3, Just 1), (4, Just 0)
            ]

    it "should be costs of edges for rays of a star" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;

            GAB.edge 2 v0 v1;
            GAB.edge 5 v0 v2;
            GAB.edge 42 v0 v3
        }

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== 0);
            distances <- Dist.dijkstra vs id;
            GAA.vgraph distances
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (0, Just 0), (1, Just 2)
            , (2, Just 5), (3, Just 42)
            ]

    it "should be the distances of the shortest paths" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;
            v4 <- GAB.vertex 4;

            GAB.edge 2 v0 v1;
            GAB.edge 2 v1 v2;
            GAB.edge 1 v2 v4;
            GAB.edge 3 v0 v3;
            GAB.edge 3 v3 v4
        }

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== 0);
            distances <- Dist.dijkstra vs id;
            GAA.vgraph distances
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (0, Just 0), (1, Just 2)
            , (2, Just 4), (3, Just 3)
            , (4, Just 5)
            ]

    it "should not be distances of shortest by edges paths" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;
            v4 <- GAB.vertex 4;
            v5 <- GAB.vertex 5;

            GAB.edge 20 v0 v1;
            GAB.edge 30 v0 v2;
            GAB.edge 2 v0 v3;
            GAB.edge 3 v3 v4;
            GAB.edge 4 v4 v2;
            GAB.edge 5 v1 v5;
            GAB.edge 1 v2 v5;
            GAB.edge 10 v4 v5;
            GAB.edge 1 v5 v0
        }

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== 0);
            distances <- Dist.dijkstra vs id;
            GAA.vgraph distances
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (0, Just 0), (1, Just 20)
            , (2, Just 9), (3, Just 2)
            , (4, Just 5), (5, Just 10)
            ]

bfordSpec :: Spec
bfordSpec = describe "bford" $ do

    it "should be Nothing for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let mg' = GAA.execute g $ do {
            distances <- Dist.bford [] (const 1);
            traverse GAA.vgraph distances
        }

        isJust mg' `shouldBe` True
        let g' = fromJust mg'

        GA.vertices g' `shouldMatchList` [Nothing, Nothing, Nothing, Nothing]

    it "should be 0 for all vertices if all vertices are start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let mg' = GAA.execute g $ do {
            vs <- GAA.vertices;
            distances <- Dist.bford vs (const 1);
            traverse GAA.vgraph distances
        }

        isJust mg' `shouldBe` True
        let g' = fromJust mg'

        GA.vertices g' `shouldMatchList` [Just 0, Just 0, Just 0, Just 0]

    it "should be 0 for a single vertex on isolated vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let mg' = GAA.execute g $ do {
            vs <- GAA.vfind (== 3);
            distances <- Dist.bford vs (const 1);
            traverse GAA.vgraph distances
        }

        isJust mg' `shouldBe` True
        let g' = fromJust mg'

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (1, Nothing), (2, Nothing)
            , (3, Just 0), (4, Nothing)
            ]

    it "should be 1 for rays of a star" $ do

        let g = GAC.star 4 [1, 2, 3]

        let mg' = GAA.execute g $ do {
            vs <- GAA.vfind (== 4);
            distances <- Dist.bford vs (const 1);
            traverse GAA.vgraph distances
        }

        isJust mg' `shouldBe` True
        let g' = fromJust mg'

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (1, Just 1), (2, Just 1)
            , (3, Just 1), (4, Just 0)
            ]

    it "should be costs of edges for rays of a star" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;

            GAB.edge 2 v0 v1;
            GAB.edge 5 v0 v2;
            GAB.edge 42 v0 v3
        }

        let mg' = GAA.execute g $ do {
            vs <- GAA.vfind (== 0);
            distances <- Dist.bford vs id;
            traverse GAA.vgraph distances
        }

        isJust mg' `shouldBe` True
        let g' = fromJust mg'

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (0, Just 0), (1, Just 2)
            , (2, Just 5), (3, Just 42)
            ]

    it "should be the distances of the shortest paths" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;
            v4 <- GAB.vertex 4;

            GAB.edge 2 v0 v1;
            GAB.edge 2 v1 v2;
            GAB.edge 1 v2 v4;
            GAB.edge 3 v0 v3;
            GAB.edge 3 v3 v4
        }

        let mg' = GAA.execute g $ do {
            vs <- GAA.vfind (== 0);
            distances <- Dist.bford vs id;
            traverse GAA.vgraph distances
        }

        isJust mg' `shouldBe` True
        let g' = fromJust mg'

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (0, Just 0), (1, Just 2)
            , (2, Just 4), (3, Just 3)
            , (4, Just 5)
            ]

    it "should not be distances of shortest by edges paths" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;
            v4 <- GAB.vertex 4;
            v5 <- GAB.vertex 5;

            GAB.edge 20 v0 v1;
            GAB.edge 30 v0 v2;
            GAB.edge 2 v0 v3;
            GAB.edge 3 v3 v4;
            GAB.edge 4 v4 v2;
            GAB.edge 5 v1 v5;
            GAB.edge 1 v2 v5;
            GAB.edge 10 v4 v5;
            GAB.edge 1 v5 v0
        }

        let mg' = GAA.execute g $ do {
            vs <- GAA.vfind (== 0);
            distances <- Dist.bford vs id;
            traverse GAA.vgraph distances
        }

        isJust mg' `shouldBe` True
        let g' = fromJust mg'

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (0, Just 0), (1, Just 20)
            , (2, Just 9), (3, Just 2)
            , (4, Just 5), (5, Just 10)
            ]

    it "should break greedy approach" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;

            GAB.edge (-1) v0 v1;
            GAB.edge (-4) v1 v2;
            GAB.edge (-3) v0 v2
        }

        let mg' = GAA.execute g $ do {
            vs <- GAA.vfind (== 0);
            distances <- Dist.bford vs id;
            traverse GAA.vgraph distances
        }

        isJust mg' `shouldBe` True
        let g' = fromJust mg'

        GA.vertices (GA.zip g g') `shouldMatchList` [(0, Just 0), (1, Just (-1)), (2, Just (-5))]

    it "should be Nothing on negative cycle" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;

            GAB.edge 1 v0 v1;
            GAB.edge 2 v1 v2;
            GAB.edge 2 v2 v3;
            GAB.edge (-5) v3 v1
        }

        let mg' = GAA.execute g $ do {
            vs <- GAA.vfind (== 0);
            distances <- Dist.bford vs id;
            traverse GAA.vgraph distances
        }

        isNothing mg' `shouldBe` True


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor.Algorithm.Distance" $ do
    dijkstraSpec
    bfordSpec
