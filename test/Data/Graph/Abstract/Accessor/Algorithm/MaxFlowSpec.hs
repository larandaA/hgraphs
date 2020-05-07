{-# LANGUAGE RankNTypes #-}

module Data.Graph.Abstract.Accessor.Algorithm.MaxFlowSpec (spec) where

import Control.Monad
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
import qualified Data.Graph.Abstract.Accessor.Algorithm.MaxFlow as MaxFlow
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import Test.Hspec

type MaxFlow s e v = GAA.Vertex s
                  -> GAA.Vertex s
                  -> (GAA.Edge s -> GAA.Accessor s e v Int)
                  -> GAA.Accessor s e v (GAA.EArray s Int)

maxflowSpec :: (forall s e v. MaxFlow s e v) -> String -> Spec
maxflowSpec algorithm name = describe name $ do

    it "should work fine on source and sink being the same vertex" $ do

        let g = GAC.path [1, 2, 3, 4]

        let flow = GAA.execute g $ do {
            source <- GAA.vfind (== 2);
            sink <- GAA.vfind (== 2);
            flow <- algorithm (head source) (head sink) (const (pure 1));
            edges <- GAA.outgoing (head source);
            flow' <- traverse (GAA.eget flow) edges;
            pure (foldr (+) 0 flow')
        }

        flow `shouldBe` 0

    it "should work fine on graph without edges" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let flow = GAA.execute g $ do {
            source <- GAA.vfind (== 1);
            sink <- GAA.vfind (== 4);
            flow <- algorithm (head source) (head sink) (const (pure 1));
            edges <- GAA.outgoing (head source);
            flow' <- traverse (GAA.eget flow) edges;
            pure (foldr (+) 0 flow')
        }

        flow `shouldBe` 0

    it "should be 0 for graph with edges having 0 capacity" $ do

        let g = GAC.path [1, 2, 3, 4]

        let (g', flow) = GAA.execute g $ do {
            source <- GAA.vfind (== 1);
            sink <- GAA.vfind (== 4);
            flow <- algorithm (head source) (head sink) (const (pure 0));
            edges <- GAA.outgoing (head source);
            flow' <- traverse (GAA.eget flow) edges;
            g' <- GAA.egraph flow;
            pure (g', foldr (+) 0 flow')
        }

        flow `shouldBe` 0
        GA.edges g' `shouldMatchList` [(1, 0, 2), (2, 0, 3), (3, 0, 4)]

    it "should be 0 for graph with source edges having 0 capacity" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;

            GAB.edge 0 v0 v1;
            GAB.edge 1 v1 v2;
            GAB.edge 1 v2 v3
        }

        let (g', flow) = GAA.execute g $ do {
            source <- GAA.vfind (== 0);
            sink <- GAA.vfind (== 3);
            flow <- algorithm (head source) (head sink) (GAA.label);
            edges <- GAA.outgoing (head source);
            flow' <- traverse (GAA.eget flow) edges;
            g' <- GAA.egraph flow;
            pure (g', foldr (+) 0 flow')
        }

        flow `shouldBe` 0
        GA.edges g' `shouldMatchList` [(0, 0, 1), (1, 0, 2), (2, 0, 3)]

    it "should be the minimum capacity for all edges" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;

            GAB.edge 2 v0 v1;
            GAB.edge 1 v1 v2;
            GAB.edge 3 v2 v3
        }

        let (g', flow) = GAA.execute g $ do {
            source <- GAA.vfind (== 0);
            sink <- GAA.vfind (== 3);
            flow <- algorithm (head source) (head sink) (GAA.label);
            edges <- GAA.outgoing (head source);
            flow' <- traverse (GAA.eget flow) edges;
            g' <- GAA.egraph flow;
            pure (g', foldr (+) 0 flow')
        }

        flow `shouldBe` 1
        GA.edges g' `shouldMatchList` [(0, 1, 1), (1, 1, 2), (2, 1, 3)]

    it "should push through multiple paths" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;
            v4 <- GAB.vertex 4;

            GAB.edge 4 v0 v1;
            GAB.edge 2 v1 v2;
            GAB.edge 3 v2 v3;
            GAB.edge 2 v3 v4;
            GAB.edge 3 v1 v4
        }

        let flow = GAA.execute g $ do {
            source <- GAA.vfind (== 0);
            sink <- GAA.vfind (== 4);
            flow <- algorithm (head source) (head sink) (GAA.label);
            edges <- GAA.outgoing (head source);
            flow' <- traverse (GAA.eget flow) edges;
            pure (foldr (+) 0 flow')
        }

        flow `shouldBe` 4

    it "should find correct flow in graph with cycles" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";
            d <- GAB.vertex "d";
            e <- GAB.vertex "e";
            f <- GAB.vertex "f";
            g <- GAB.vertex "g";
            h <- GAB.vertex "h";

            GAB.edge 3 a b;
            GAB.edge 3 a d;
            GAB.edge 4 b c;
            GAB.edge 3 c a;
            GAB.edge 1 c d;
            GAB.edge 2 c e;
            GAB.edge 2 d e;
            GAB.edge 6 d h;
            GAB.edge 6 h f;
            GAB.edge 1 e b;
            GAB.edge 1 e g;
            GAB.edge 9 f g
        }

        let (g', flow) = GAA.execute g $ do {
            source <- GAA.vfind (== "a");
            sink <- GAA.vfind (== "g");
            flow <- algorithm (head source) (head sink) (GAA.label);
            edges <- GAA.outgoing (head source);
            flow' <- traverse (GAA.eget flow) edges;
            g' <- GAA.egraph flow;
            pure (g', foldr (+) 0 flow')
        }

        flow `shouldBe` 5
        GA.edges g' `shouldMatchList`
            [ ("a", 2, "b"), ("a", 3, "d")
            , ("b", 2, "c")
            , ("c", 0, "a"), ("c", 1, "d"), ("c", 1, "e")
            , ("d", 0, "e"), ("d", 4, "h")
            , ("e", 0, "b"), ("e", 1, "g")
            , ("f", 4, "g")
            , ("h", 4, "f")
            ]

    it "should find correct flow in graph multiple shortest paths" $ do

        let g = GAB.build $ do {
            s <- GAB.vertex "s";
            v1 <- GAB.vertex "1";
            v2 <- GAB.vertex "2";
            v3 <- GAB.vertex "3";
            v4 <- GAB.vertex "4";
            t <- GAB.vertex "t";

            GAB.edge 10 s v1;
            GAB.edge 10 s v2;
            GAB.edge 2 v1 v2;
            GAB.edge 4 v1 v3;
            GAB.edge 8 v1 v4;
            GAB.edge 9 v2 v4;
            GAB.edge 10 v3 t;
            GAB.edge 6 v4 v3;
            GAB.edge 10 v4 t
        }

        let flow = GAA.execute g $ do {
            source <- GAA.vfind (== "s");
            sink <- GAA.vfind (== "t");
            flow <- algorithm (head source) (head sink) (GAA.label);
            edges <- GAA.outgoing (head source);
            flow' <- traverse (GAA.eget flow) edges;
            pure (foldr (+) 0 flow')
        }

        flow `shouldBe` 19


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor.Algorithm.MaxFlow" $ do
    maxflowSpec MaxFlow.edkarp "edkarp"
    maxflowSpec MaxFlow.dinic "dinic"
