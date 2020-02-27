module Data.GraphSpec (spec) where

import Control.Monad.ST
import qualified Data.List as L
import qualified Data.Graph.Abstract as GA
import Test.Hspec


verticesSpec :: Spec
verticesSpec = describe "vertices" $ do

    it "should be empty for empty graph" $ do

        let g = GA.build (pure ())

        GA.vertices g `shouldBe` ([] :: [()])

    it "should return all vertices for graph without edges" $ do

        let g = GA.build $ do {
            GA.vertex "a";
            GA.vertex "b";
            pure ()
        }

        GA.vertices g `shouldMatchList` ["a", "b"]

    it "should return vertices with repeated labels" $ do

        let g = GA.build $ do {
            GA.vertex "a";
            GA.vertex "b";
            GA.vertex "a";
            pure ()
        }

        GA.vertices g `shouldMatchList` ["a", "a", "b"]

    it "should return all vertices for graph with edges" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            GA.edge a b ()
        }

        GA.vertices g `shouldMatchList` ["a", "b"]

edgesSpec :: Spec
edgesSpec = describe "edges" $ do

    it "should be empty for empty graph" $ do

        let g = GA.build (pure ())

        GA.edges g `shouldBe` ([] :: [((), (), ())])

    it "should be empty for graph without edges" $ do

        let g = GA.build $ do {
            GA.vertex "a";
            GA.vertex "b";
            pure ()
        }

        GA.edges g `shouldBe` ([] :: [(String, (), String)])

    it "should return all edges for graph with edges" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";
            GA.edge a b ();
            GA.edge a c ();
            GA.edge b c ()
        }

        GA.edges g `shouldMatchList` [("a", (), "b"), ("a", (), "c"), ("b", (), "c")]

buildSpec :: Spec
buildSpec = describe "build" $ do

    it "should create empty graph" $ do

        let g = GA.build (pure ())

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should create graph with all verteces isolated" $ do

        let g = GA.build $ do {
            GA.vertex ();
            GA.vertex ();
            pure ()
        }

        length (GA.vertices g) `shouldBe` 2
        length (GA.edges g) `shouldBe` 0

    it "should create graph with different vertex degrees" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge b a ();
            GA.edge c a ();
            GA.edge c b ()
        }

        length (GA.vertices g) `shouldBe` 3
        length (GA.edges g) `shouldBe` 3
        GA.vertices (GA.zip g (GA.degree g)) `shouldMatchList` [("a", 0), ("b", 1), ("c", 2)]

vmapSpec :: Spec
vmapSpec = describe "vmap" $ do

    it "should preserve empty graph" $ do

        let g = GA.vmap (+ 1) (GA.build (pure ()))

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should not change graph with id function" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge b a "e1";
            GA.edge c a "e2";
            GA.edge c b "e3"
        }
        let g' = GA.vmap id g

        GA.vertices g' `shouldMatchList` ["a", "b", "c"]
        GA.edges g' `shouldMatchList` [("b", "e1", "a"), ("c", "e2", "a"), ("c", "e3", "b")]

    it "should not change graph structure" $ do

        let g = GA.build $ do {
            v0 <- GA.vertex 0;
            v1 <- GA.vertex 1;
            v2 <- GA.vertex 2;

            GA.edge v1 v0 "e1";
            GA.edge v2 v0 "e2";
            GA.edge v2 v1 "e3"
        }
        let g' = GA.vmap (+ 1) g

        GA.vertices g' `shouldMatchList` [1, 2, 3]
        GA.edges g' `shouldMatchList` [(2, "e1", 1), (3, "e2", 1), (3, "e3", 2)]

emapSpec :: Spec
emapSpec = describe "emap" $ do

    it "should preserve empty graph" $ do

        let g = GA.emap (+ 1) (GA.build (pure ()))

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should not change graph with id function" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge b a "e1";
            GA.edge c a "e2";
            GA.edge c b "e3"
        }
        let g' = GA.emap id g

        GA.vertices g' `shouldMatchList` ["a", "b", "c"]
        GA.edges g' `shouldMatchList` [("b", "e1", "a"), ("c", "e2", "a"), ("c", "e3", "b")]


    it "should not change graph structure" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge b a 0;
            GA.edge c a 1;
            GA.edge c b 2
        }
        let g' = GA.emap (+ 1) g

        GA.vertices g' `shouldMatchList` ["a", "b", "c"]
        GA.edges g' `shouldMatchList` [("b", 1, "a"), ("c", 2, "a"), ("c", 3, "b")]

emapcSpec :: Spec
emapcSpec = describe "emapc" $ do

    it "should preserve empty graph" $ do

        let g = GA.emapc (\_ e _ -> e) (GA.build (pure ()))

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should not change graph with id function" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge b a "e1";
            GA.edge c a "e2";
            GA.edge c b "e3"
        }
        let g' = GA.emapc (\_ e _ -> e) g

        GA.vertices g' `shouldMatchList` ["a", "b", "c"]
        GA.edges g' `shouldMatchList` [("b", "e1", "a"), ("c", "e2", "a"), ("c", "e3", "b")]


    it "should not change graph structure" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge b a ();
            GA.edge c a ();
            GA.edge c b ();
            GA.edge a b ();
            GA.edge a c ();
            GA.edge b c ()
        }
        let reverseEdge v1 _ v2 = v1 > v2
        let g' = GA.emapc reverseEdge g

        GA.vertices g' `shouldMatchList` ["a", "b", "c"]
        GA.edges g' `shouldMatchList`
            [ ("b", True, "a")
            , ("c", True, "a")
            , ("c", True, "b")
            , ("a", False, "b")
            , ("a", False, "c")
            , ("b", False, "c")
            ]

zipSpec :: Spec
zipSpec = describe "zip" $ do

    it "should result in empty graph" $ do

        let g1 = GA.build (pure ())
        let g2 = GA.build (pure ())
        let g = GA.zip g1 g2

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should zip graphs without edges" $ do

        let g1 = GA.build $ do {
            GA.vertex "a";
            GA.vertex "b";
            pure ()
        }
        let g2 = GA.build $ do {
            GA.vertex 1;
            GA.vertex 2;
            pure ()
        }
        let g = GA.zip g1 g2

        GA.vertices g `shouldMatchList` [("a", 1), ("b", 2)]
        length (GA.edges g) `shouldBe` 0

    it "should zip graphs with edges" $ do

        let g1 = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge a b "e1";
            GA.edge b c "e2"

        }
        let g2 = GA.build $ do {
            v0 <- GA.vertex 0;
            v1 <- GA.vertex 1;
            v2 <- GA.vertex 2;

            GA.edge v0 v1 "01";
            GA.edge v1 v2 "12"
        }
        let g = GA.zip g1 g2

        GA.vertices g `shouldMatchList` [("a", 0), ("b", 1), ("c", 2)]
        GA.edges g `shouldMatchList`
            [ (("a", 0), ("e1", "01"), ("b", 1))
            , (("b", 1), ("e2", "12"), ("c", 2))
            ]

succsSpec :: Spec
succsSpec = describe "succs" $ do

    it "should create empty graph" $ do

        let g = GA.succs (GA.build (pure ()))

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should be empty for isolated vertices" $ do

        let g = GA.build $ do {
            GA.vertex ();
            GA.vertex ();
            pure ()
        }
        let g' = GA.succs g

        GA.vertices g' `shouldBe` ([[], []] :: [[((), ())]])
        length (GA.edges g) `shouldBe` 0

    it "should preserve graph structure" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge b a "ba";
            GA.edge c a "ca";
            GA.edge c b "cb"
        }
        let g' = GA.vmap L.sort (GA.succs g)

        GA.vertices (GA.zip g g') `shouldMatchList` 
            [ ("a", [])
            , ("b", [("ba", "a")])
            , ("c", [("ca", "a"), ("cb", "b")])
            ]
        GA.edges g' `shouldMatchList` 
            [ ([("ba", "a")], "ba", [])
            , ([("ca", "a"), ("cb", "b")], "ca", [])
            , ([("ca", "a"), ("cb", "b")], "cb", [("ba", "a")])
            ]

predsSpec :: Spec
predsSpec = describe "preds" $ do

    it "should create empty graph" $ do

        let g = GA.preds (GA.build (pure ()))

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should be empty for isolated vertices" $ do

        let g = GA.build $ do {
            GA.vertex ();
            GA.vertex ();
            pure ()
        }
        let g' = GA.preds g

        GA.vertices g' `shouldBe` ([[], []] :: [[((), ())]])
        length (GA.edges g) `shouldBe` 0

    it "should preserve graph structure" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge a b "ab";
            GA.edge a c "ac";
            GA.edge b c "bc"
        }
        let g' = GA.vmap L.sort (GA.preds g)

        GA.vertices (GA.zip g' g) `shouldMatchList` 
            [ ([], "a")
            , ([("a", "ab")], "b")
            , ([("a", "ac"), ("b", "bc")], "c")
            ]
        GA.edges g' `shouldMatchList` 
            [ ([], "ab", [("a", "ab")])
            , ([], "ac", [("a", "ac"), ("b", "bc")])
            , ([("a", "ab")], "bc", [("a", "ac"), ("b", "bc")])
            ]

transposeSpec :: Spec
transposeSpec = describe "transpose" $ do

    it "should create empty graph" $ do

        let g = GA.transpose (GA.build (pure ()))

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should not change graph without edges" $ do

        let g = GA.build $ do {
            GA.vertex "v";
            GA.vertex "u";
            GA.vertex "w";
            pure ()
        }
        let g' = GA.transpose g

        GA.vertices (GA.zip g g') `shouldMatchList` [("v", "v"), ("u", "u"), ("w", "w")]
        length (GA.edges g) `shouldBe` 0

    it "should reverse edges" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge a b "ab";
            GA.edge a c "ac";
            GA.edge b c "bc"
        }
        let g' = GA.transpose g

        GA.vertices g' `shouldMatchList` ["a", "b", "c"]
        GA.edges g' `shouldMatchList` 
            [ ("b", "ab", "a")
            , ("c", "ac", "a")
            , ("c", "bc", "b")
            ]

transformuSpec :: Spec
transformuSpec = describe "transformu" $ do

    it "should transform an empty graph to an empty graph" $ do

        let g = GA.transformu (const True) (\_ _ -> ()) (const ()) (GA.build (pure ()))

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices to default values" $ do

        let g = GA.build $ do {
            GA.vertex "v";
            GA.vertex "u";
            GA.vertex "w";
            pure ()
        }
        let g' = GA.transformu (const False) (\_ _ -> 0) (const 1) g

        GA.vertices g' `shouldMatchList` [1, 1, 1]
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices to non-default values" $ do

        let g = GA.build $ do {
            GA.vertex "v";
            GA.vertex "u";
            GA.vertex "w";
            pure ()
        }
        let g' = GA.transformu (const True) (\_ _ -> 0) (const 1) g

        GA.vertices g' `shouldMatchList` [0, 0, 0]
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices from a component to non-default values" $ do

        let g = GA.build $ do {
            v <- GA.vertex "v";
            u <- GA.vertex "u";
            w <- GA.vertex "w";
            GA.edge v w "vw"
        }
        let g' = GA.transformu (== "v") (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("v", 0), ("u", 1), ("w", 0)]
        GA.edges (GA.zip g g') `shouldBe` [(("v", 0), ("vw", "vw"), ("w", 0))]

    it "should transform all vertices to number of all processed successors" $ do

        let g = GA.build $ do {
            v <- GA.vertex "v";
            u <- GA.vertex "u";
            w <- GA.vertex "w";
            t <- GA.vertex "t";
            r <- GA.vertex "r";
            k <- GA.vertex "k";

            GA.edge v w "vw";
            GA.edge u t "ut";
            GA.edge w t "wt";
            GA.edge w r "wr";
            GA.edge t u "tu";
            GA.edge t k "tk"
        }
        let isStart = (`elem` ["v", "u"])
        let h = (\_ -> (+ 1) . L.sum . L.map snd)
        let g' = GA.transformu isStart h (const 0) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("v", 3), ("u", 3), ("w", 2), ("r", 1), ("t", 2), ("k", 1)]

transformdSpec :: Spec
transformdSpec = describe "transformd" $ do

    it "should transform an empty graph to an empty graph" $ do

        let g = GA.transformd (const True) (\_ _ -> ()) (const ()) (GA.build (pure ()))

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices to default values" $ do

        let g = GA.build $ do {
            GA.vertex "v";
            GA.vertex "u";
            GA.vertex "w";
            pure ()
        }
        let g' = GA.transformd (const False) (\_ _ -> 0) (const 1) g

        GA.vertices g' `shouldMatchList` [1, 1, 1]
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices to non-default values" $ do

        let g = GA.build $ do {
            GA.vertex "v";
            GA.vertex "u";
            GA.vertex "w";
            pure ()
        }
        let g' = GA.transformd (const True) (\_ _ -> 0) (const 1) g

        GA.vertices g' `shouldMatchList` [0, 0, 0]
        length (GA.edges g) `shouldBe` 0

    it "should transform all vertices from a component to non-default values" $ do

        let g = GA.build $ do {
            v <- GA.vertex "v";
            u <- GA.vertex "u";
            w <- GA.vertex "w";
            GA.edge v w "vw"
        }
        let g' = GA.transformd (== "v") (\_ _ -> 0) (const 1) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("v", 0), ("u", 1), ("w", 0)]
        GA.edges (GA.zip g g') `shouldBe` [(("v", 0), ("vw", "vw"), ("w", 0))]

    it "should transform all vertices to distance from source" $ do

        let g = GA.build $ do {
            v <- GA.vertex "v";
            u <- GA.vertex "u";
            w <- GA.vertex "w";
            t <- GA.vertex "t";
            r <- GA.vertex "r";
            k <- GA.vertex "k";

            GA.edge v w "vw";
            GA.edge u t "ut";
            GA.edge w t "wt";
            GA.edge w r "wr";
            GA.edge t u "tu";
            GA.edge t k "tk"
        }
        let isStart = (`elem` ["v", "u"])
        let d = (\preds _ -> (+ 1) . L.sum . L.map fst $ preds)
        let g' = GA.transformd isStart d (const 0) g

        GA.vertices (GA.zip g g') `shouldMatchList` [("v", 1), ("u", 1), ("w", 2), ("r", 3), ("t", 2), ("k", 3)]


spec :: Spec
spec = describe "Data.GA.aph" $ do
    buildSpec
    verticesSpec
    edgesSpec
    vmapSpec
    emapSpec
    emapcSpec
    zipSpec
    succsSpec
    predsSpec
    transposeSpec
    transformuSpec
    transformdSpec
