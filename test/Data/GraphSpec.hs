module Data.GraphSpec (spec) where

import Control.Monad.ST
import qualified Data.List as L
import qualified Data.Graph as G
import Test.Hspec


verticesSpec :: Spec
verticesSpec = describe "vertices" $ do

    it "should be empty for empty graph" $ do

        let g = G.build (pure ())

        G.vertices g `shouldBe` ([] :: [()])

    it "should return all vertices for graph without edges" $ do

        let g = G.build $ do {
            G.vertex "a";
            G.vertex "b";
            pure ()
        }

        G.vertices g `shouldMatchList` ["a", "b"]

    it "should return vertices with repeated labels" $ do

        let g = G.build $ do {
            G.vertex "a";
            G.vertex "b";
            G.vertex "a";
            pure ()
        }

        G.vertices g `shouldMatchList` ["a", "a", "b"]

    it "should return all vertices for graph with edges" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            G.edge a b ()
        }

        G.vertices g `shouldMatchList` ["a", "b"]

edgesSpec :: Spec
edgesSpec = describe "edges" $ do

    it "should be empty for empty graph" $ do

        let g = G.build (pure ())

        G.edges g `shouldBe` ([] :: [((), (), ())])

    it "should be empty for graph without edges" $ do

        let g = G.build $ do {
            G.vertex "a";
            G.vertex "b";
            pure ()
        }

        G.edges g `shouldBe` ([] :: [(String, (), String)])

    it "should return all edges for graph with edges" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            c <- G.vertex "c";
            G.edge a b ();
            G.edge a c ();
            G.edge b c ()
        }

        G.edges g `shouldMatchList` [("a", (), "b"), ("a", (), "c"), ("b", (), "c")]

buildSpec :: Spec
buildSpec = describe "build" $ do

    it "should create empty graph" $ do

        let g = G.build (pure ())

        length (G.vertices g) `shouldBe` 0
        length (G.edges g) `shouldBe` 0

    it "should create graph with all verteces isolated" $ do

        let g = G.build $ do {
            G.vertex ();
            G.vertex ();
            pure ()
        }

        length (G.vertices g) `shouldBe` 2
        length (G.edges g) `shouldBe` 0

    it "should create graph with different vertex degrees" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            c <- G.vertex "c";

            G.edge b a ();
            G.edge c a ();
            G.edge c b ()
        }

        length (G.vertices g) `shouldBe` 3
        length (G.edges g) `shouldBe` 3
        G.vertices (G.zip g (G.degree g)) `shouldMatchList` [("a", 0), ("b", 1), ("c", 2)]

vmapSpec :: Spec
vmapSpec = describe "vmap" $ do

    it "should preserve empty graph" $ do

        let g = G.vmap (+ 1) (G.build (pure ()))

        length (G.vertices g) `shouldBe` 0
        length (G.edges g) `shouldBe` 0

    it "should not change graph with id function" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            c <- G.vertex "c";

            G.edge b a "e1";
            G.edge c a "e2";
            G.edge c b "e3"
        }
        let g' = G.vmap id g

        G.vertices g' `shouldMatchList` ["a", "b", "c"]
        G.edges g' `shouldMatchList` [("b", "e1", "a"), ("c", "e2", "a"), ("c", "e3", "b")]

    it "should not change graph structure" $ do

        let g = G.build $ do {
            v0 <- G.vertex 0;
            v1 <- G.vertex 1;
            v2 <- G.vertex 2;

            G.edge v1 v0 "e1";
            G.edge v2 v0 "e2";
            G.edge v2 v1 "e3"
        }
        let g' = G.vmap (+ 1) g

        G.vertices g' `shouldMatchList` [1, 2, 3]
        G.edges g' `shouldMatchList` [(2, "e1", 1), (3, "e2", 1), (3, "e3", 2)]

emapSpec :: Spec
emapSpec = describe "emap" $ do

    it "should preserve empty graph" $ do

        let g = G.emap (+ 1) (G.build (pure ()))

        length (G.vertices g) `shouldBe` 0
        length (G.edges g) `shouldBe` 0

    it "should not change graph with id function" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            c <- G.vertex "c";

            G.edge b a "e1";
            G.edge c a "e2";
            G.edge c b "e3"
        }
        let g' = G.emap id g

        G.vertices g' `shouldMatchList` ["a", "b", "c"]
        G.edges g' `shouldMatchList` [("b", "e1", "a"), ("c", "e2", "a"), ("c", "e3", "b")]


    it "should not change graph structure" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            c <- G.vertex "c";

            G.edge b a 0;
            G.edge c a 1;
            G.edge c b 2
        }
        let g' = G.emap (+ 1) g

        G.vertices g' `shouldMatchList` ["a", "b", "c"]
        G.edges g' `shouldMatchList` [("b", 1, "a"), ("c", 2, "a"), ("c", 3, "b")]

zipSpec :: Spec
zipSpec = describe "zip" $ do

    it "should result in empty graph" $ do

        let g1 = G.build (pure ())
        let g2 = G.build (pure ())
        let g = G.zip g1 g2

        length (G.vertices g) `shouldBe` 0
        length (G.edges g) `shouldBe` 0

    it "should zip graphs without edges" $ do

        let g1 = G.build $ do {
            G.vertex "a";
            G.vertex "b";
            pure ()
        }
        let g2 = G.build $ do {
            G.vertex 1;
            G.vertex 2;
            pure ()
        }
        let g = G.zip g1 g2

        G.vertices g `shouldMatchList` [("a", 1), ("b", 2)]
        length (G.edges g) `shouldBe` 0

    it "should zip graphs with edges" $ do

        let g1 = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            c <- G.vertex "c";

            G.edge a b "e1";
            G.edge b c "e2"

        }
        let g2 = G.build $ do {
            v0 <- G.vertex 0;
            v1 <- G.vertex 1;
            v2 <- G.vertex 2;

            G.edge v0 v1 "01";
            G.edge v1 v2 "12"
        }
        let g = G.zip g1 g2

        G.vertices g `shouldMatchList` [("a", 0), ("b", 1), ("c", 2)]
        G.edges g `shouldMatchList`
            [ (("a", 0), ("e1", "01"), ("b", 1))
            , (("b", 1), ("e2", "12"), ("c", 2))
            ]

succsSpec :: Spec
succsSpec = describe "succs" $ do

    it "should create empty graph" $ do

        let g = G.succs (G.build (pure ()))

        length (G.vertices g) `shouldBe` 0
        length (G.edges g) `shouldBe` 0

    it "should be empty for isolated vertices" $ do

        let g = G.build $ do {
            G.vertex ();
            G.vertex ();
            pure ()
        }
        let g' = G.succs g

        G.vertices g' `shouldBe` ([[], []] :: [[((), ())]])
        length (G.edges g) `shouldBe` 0

    it "should preserve graph structure" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            c <- G.vertex "c";

            G.edge b a "ba";
            G.edge c a "ca";
            G.edge c b "cb"
        }
        let g' = G.vmap L.sort (G.succs g)

        G.vertices (G.zip g g') `shouldMatchList` 
            [ ("a", [])
            , ("b", [("ba", "a")])
            , ("c", [("ca", "a"), ("cb", "b")])
            ]
        G.edges g' `shouldMatchList` 
            [ ([("ba", "a")], "ba", [])
            , ([("ca", "a"), ("cb", "b")], "ca", [])
            , ([("ca", "a"), ("cb", "b")], "cb", [("ba", "a")])
            ]

predsSpec :: Spec
predsSpec = describe "preds" $ do

    it "should create empty graph" $ do

        let g = G.preds (G.build (pure ()))

        length (G.vertices g) `shouldBe` 0
        length (G.edges g) `shouldBe` 0

    it "should be empty for isolated vertices" $ do

        let g = G.build $ do {
            G.vertex ();
            G.vertex ();
            pure ()
        }
        let g' = G.preds g

        G.vertices g' `shouldBe` ([[], []] :: [[((), ())]])
        length (G.edges g) `shouldBe` 0

    it "should preserve graph structure" $ do

        let g = G.build $ do {
            a <- G.vertex "a";
            b <- G.vertex "b";
            c <- G.vertex "c";

            G.edge a b "ab";
            G.edge a c "ac";
            G.edge b c "bc"
        }
        let g' = G.vmap L.sort (G.preds g)

        G.vertices (G.zip g' g) `shouldMatchList` 
            [ ([], "a")
            , ([("a", "ab")], "b")
            , ([("a", "ac"), ("b", "bc")], "c")
            ]
        G.edges g' `shouldMatchList` 
            [ ([], "ab", [("a", "ab")])
            , ([], "ac", [("a", "ac"), ("b", "bc")])
            , ([("a", "ab")], "bc", [("a", "ac"), ("b", "bc")])
            ]

spec :: Spec
spec = describe "Data.Graph" $ do
    buildSpec
    verticesSpec
    edgesSpec
    vmapSpec
    emapSpec
    zipSpec
    succsSpec
    predsSpec
