module Data.Graph.Abstract.TransformSpec (spec) where

import qualified Data.List as L
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import qualified Data.Graph.Abstract.Transform as GAT
import Test.Hspec


transformuSpec :: Spec
transformuSpec = describe "transformu" $ do

    it "should transform an empty graph to an empty graph" $ do

        let g = GAT.transformu (const True) (\_ _ -> ()) (const ()) GAC.empty

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices to default values" $ do

        let g = GAB.build $ do {
            GAB.vertex "v";
            GAB.vertex "u";
            GAB.vertex "w";
            pure ()
        }
        let g' = GAT.transformu (const False) (\_ _ -> 0) (const 1) g

        GA.vertices g' `shouldMatchList` [1, 1, 1]
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices to non-default values" $ do

        let g = GAB.build $ do {
            GAB.vertex "v";
            GAB.vertex "u";
            GAB.vertex "w";
            pure ()
        }
        let g' = GAT.transformu (const True) (\_ _ -> 0) (const 1) g

        GA.vertices g' `shouldMatchList` [0, 0, 0]
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices from a component to non-default values" $ do

        let g = GAB.build $ do {
            v <- GAB.vertex "v";
            u <- GAB.vertex "u";
            w <- GAB.vertex "w";
            GAB.edge "vw" v w
        }
        let g' = GAT.transformu (== "v") (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("v", 0), ("u", 1), ("w", 0)]
        GA.edges (GA.zip g g') `shouldBe` [(("v", 0), ("vw", "vw"), ("w", 0))]

    it "should transform all vertices to number of all processed successors" $ do

        let g = GAB.build $ do {
            v <- GAB.vertex "v";
            u <- GAB.vertex "u";
            w <- GAB.vertex "w";
            t <- GAB.vertex "t";
            r <- GAB.vertex "r";
            k <- GAB.vertex "k";

            GAB.edge "vw" v w;
            GAB.edge "ut" u t;
            GAB.edge "wt" w t;
            GAB.edge "wr" w r;
            GAB.edge "tu" t u;
            GAB.edge "tk" t k
        }
        let isStart = (`elem` ["v", "u"])
        let h = (\_ -> (+ 1) . L.sum . L.map snd)
        let g' = GAT.transformu isStart h (const 0) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("v", 3), ("u", 3), ("w", 2), ("r", 1), ("t", 2), ("k", 1)]

transformdSpec :: Spec
transformdSpec = describe "transformd" $ do

    it "should transform an empty graph to an empty graph" $ do

        let g = GAT.transformd (const True) (\_ _ -> ()) (const ()) GAC.empty

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices to default values" $ do

        let g = GAB.build $ do {
            GAB.vertex "v";
            GAB.vertex "u";
            GAB.vertex "w";
            pure ()
        }
        let g' = GAT.transformd (const False) (\_ _ -> 0) (const 1) g

        GA.vertices g' `shouldMatchList` [1, 1, 1]
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices to non-default values" $ do

        let g = GAB.build $ do {
            GAB.vertex "v";
            GAB.vertex "u";
            GAB.vertex "w";
            pure ()
        }
        let g' = GAT.transformd (const True) (\_ _ -> 0) (const 1) g

        GA.vertices g' `shouldMatchList` [0, 0, 0]
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices from a component to non-default values" $ do

        let g = GAB.build $ do {
            v <- GAB.vertex "v";
            u <- GAB.vertex "u";
            w <- GAB.vertex "w";
            GAB.edge "vw" v w
        }
        let g' = GAT.transformd (== "v") (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("v", 0), ("u", 1), ("w", 0)]
        GA.edges (GA.zip g g') `shouldBe` [(("v", 0), ("vw", "vw"), ("w", 0))]

    it "should transform all vertices to distance from source" $ do

        let g = GAB.build $ do {
            v <- GAB.vertex "v";
            u <- GAB.vertex "u";
            w <- GAB.vertex "w";
            t <- GAB.vertex "t";
            r <- GAB.vertex "r";
            k <- GAB.vertex "k";

            GAB.edge "vw" v w;
            GAB.edge "ut" u t;
            GAB.edge "wt" w t;
            GAB.edge "wr" w r;
            GAB.edge "tu" t u;
            GAB.edge "tk" t k
        }
        let isStart = (`elem` ["v", "u"])
        let d = (\preds _ -> (+ 1) . L.sum . L.map fst $ preds)
        let g' = GAT.transformd isStart d (const 0) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("v", 1), ("u", 1), ("w", 2), ("r", 3), ("t", 2), ("k", 3)]


spec :: Spec
spec = describe "Data.Graph.Abstract.Transform" $ do
    transformuSpec
    transformdSpec
