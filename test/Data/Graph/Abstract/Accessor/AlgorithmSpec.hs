module Data.Graph.Abstract.Accessor.AlgorithmSpec (spec) where

import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
import qualified Data.Graph.Abstract.Accessor.Algorithm as Alg
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import Test.Hspec


distancesSpec :: Spec
distancesSpec = describe "distances" $ do

    it "should be Nothing for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            distances <- Alg.distances [] (const 1);
            GAA.vgraph distances
        }

        GA.vertices g' `shouldMatchList` [Nothing, Nothing, Nothing, Nothing]

    it "should be 0 for all vertices if all vertices are start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            distances <- Alg.distances vs (const 1);
            GAA.vgraph distances
        }

        GA.vertices g' `shouldMatchList` [Just 0, Just 0, Just 0, Just 0]

    it "should be 0 for a single vertex on isolated vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== 3);
            distances <- Alg.distances vs (const 1);
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
            distances <- Alg.distances vs (const 1);
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
            distances <- Alg.distances vs id;
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
            distances <- Alg.distances vs id;
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
            distances <- Alg.distances vs id;
            GAA.vgraph distances
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (0, Just 0), (1, Just 20)
            , (2, Just 9), (3, Just 2)
            , (4, Just 5), (5, Just 10)
            ]


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor.Algorithm" $ do
    distancesSpec
