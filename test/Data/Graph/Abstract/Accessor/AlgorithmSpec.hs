module Data.Graph.Abstract.Accessor.AlgorithmSpec (spec) where

import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
import qualified Data.Graph.Abstract.Accessor.Algorithm as Alg
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import Test.Hspec


distances'Spec :: Spec
distances'Spec = describe "distances'" $ do

    it "should be Nothing for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            distances <- Alg.distances' [];
            GAA.vgraph distances
        }

        GA.vertices g' `shouldMatchList` [Nothing, Nothing, Nothing, Nothing]

    it "should be 0 for all vertices if all vertices are start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            distances <- Alg.distances' vs;
            GAA.vgraph distances
        }

        GA.vertices g' `shouldMatchList` [Just 0, Just 0, Just 0, Just 0]

    it "should be 0 for a single vertex on isolated vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== 3);
            distances <- Alg.distances' vs;
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
            distances <- Alg.distances' vs;
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
            distances <- Alg.distances' vs;
            GAA.vgraph distances
        }

        GA.vertices (GA.zip g g') `shouldMatchList`
            [ (0, Just 0), (1, Just 1)
            , (2, Just 2), (3, Just 1)
            , (4, Just 2)
            ]


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor.Algorithm" $ do
    distances'Spec
