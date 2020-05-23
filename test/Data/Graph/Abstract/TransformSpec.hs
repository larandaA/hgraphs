module Data.Graph.Abstract.TransformSpec (spec) where

import qualified Data.List as L
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import qualified Data.Graph.Abstract.Transform as GAT
import Test.Hspec

transformu'Spec :: Spec
transformu'Spec = describe "transformu'" $ do

    it "should visit only center of a star with all edges disallowed" $ do

        let g = GAC.star "a" ["b", "c", "d"]
        let g' = GAT.transformu' (== "a") (const False) (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 0), ("b", 1), ("c", 1), ("d", 1)]

    it "should visit only ray vertices with allowed edges in a star" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";
            d <- GAB.vertex "d";
            e <- GAB.vertex "e";
            GAB.edge True a b;
            GAB.edge False a c;
            GAB.edge True a d;
            GAB.edge False a e
        }
        let g' = GAT.transformu' (== "a") id (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 0), ("b", 0), ("c", 1), ("d", 0), ("e", 1)]

    it "should visit a tail of a path witha disallowed edge in the middle" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";
            d <- GAB.vertex "d";
            e <- GAB.vertex "e";
            GAB.edge True a b;
            GAB.edge True b c;
            GAB.edge False c d;
            GAB.edge True d e
        }
        let g' = GAT.transformu' (== "a") id (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 0), ("b", 0), ("c", 0), ("d", 1), ("e", 1)]

    it "should visit a vertex with at least one allowed path" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";
            d <- GAB.vertex "d";
            GAB.edge True a b;
            GAB.edge True b c;
            GAB.edge False a c;
            GAB.edge False a d;
            GAB.edge False c d
        }
        let g' = GAT.transformu' (== "a") id (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 0), ("b", 0), ("c", 0), ("d", 1)]

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

transformd'Spec :: Spec
transformd'Spec = describe "transformd'" $ do

    it "should visit only center of a star with all edges disallowed" $ do

        let g = GAC.star "a" ["b", "c", "d"]
        let g' = GAT.transformd' (== "a") (const False) (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 0), ("b", 1), ("c", 1), ("d", 1)]

    it "should visit only ray vertices with allowed edges in a star" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";
            d <- GAB.vertex "d";
            e <- GAB.vertex "e";
            GAB.edge True a b;
            GAB.edge False a c;
            GAB.edge True a d;
            GAB.edge False a e
        }
        let g' = GAT.transformd' (== "a") id (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 0), ("b", 0), ("c", 1), ("d", 0), ("e", 1)]

    it "should visit a tail of a path witha disallowed edge in the middle" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";
            d <- GAB.vertex "d";
            e <- GAB.vertex "e";
            GAB.edge True a b;
            GAB.edge True b c;
            GAB.edge False c d;
            GAB.edge True d e
        }
        let g' = GAT.transformd' (== "a") id (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 0), ("b", 0), ("c", 0), ("d", 1), ("e", 1)]

    it "should visit a vertex with at least one allowed path" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";
            d <- GAB.vertex "d";
            GAB.edge True a b;
            GAB.edge True b c;
            GAB.edge False a c;
            GAB.edge False a d;
            GAB.edge False c d
        }
        let g' = GAT.transformd' (== "a") id (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 0), ("b", 0), ("c", 0), ("d", 1)]

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
    transformu'Spec
    transformuSpec
    transformd'Spec
    transformdSpec
