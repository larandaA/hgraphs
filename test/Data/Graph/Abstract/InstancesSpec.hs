module Data.Graph.Abstract.InstancesSpec (spec) where

import qualified Data.Maybe as M
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import qualified Data.Graph.Abstract.Instances as GAI
import qualified Data.List as L
import Test.Hspec


flattenSpec :: Spec
flattenSpec = describe "flatten" $ do

    it "should convert an empty graph into an empty graph" $ do

        let g = GAC.empty
        let g' = GAI.flatten g

        length (GA.vertices g') `shouldBe` 0
        length (GA.edges g') `shouldBe` 0

    it "should convert a graph of empty graphs into an empty graph" $ do

        let g1 = GAC.empty
        let g2 = GAC.empty
        let g3 = GAC.empty
        let g' = GAB.build $ do {
            GAB.vertex g1;
            GAB.vertex g2;
            GAB.vertex g3;
            pure ()
        }

        let g'' = GAI.flatten g'

        length (GA.vertices g'') `shouldBe` 0
        length (GA.edges g'') `shouldBe` 0

    it "should convert a graph of graphs of isolated vertices into a graph of isolated vertices" $ do

        let g1 = GAB.build $ do {
            GAB.vertex 1;
            GAB.vertex 2;
            GAB.vertex 3;
            pure ()
        }

        let g2 = GAB.build $ do {
            GAB.vertex 4;
            GAB.vertex 5;
            GAB.vertex 6;
            pure ()
        }

        let g3 = GAB.build $ do {
            GAB.vertex 7;
            GAB.vertex 8;
            GAB.vertex 9;
            pure ()
        }

        let g' = GAB.build $ do {
            GAB.vertex g1;
            GAB.vertex g2;
            GAB.vertex g3;
            pure ()
        }

        let g'' = GAI.flatten g'

        GA.vertices g'' `shouldMatchList` [1, 2, 3, 4, 5, 6, 7, 8, 9]
        length (GA.edges g'') `shouldBe` 0

    it "should preserve edges within graphs" $ do

        let g1 = GAB.build $ do {
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            v3 <- GAB.vertex 3;
            GAB.edge 12 v1 v2;
            GAB.edge 23 v2 v3;
            pure ()
        }

        let g2 = GAB.build $ do {
            v4 <- GAB.vertex 4;
            v5 <- GAB.vertex 5;
            v6 <- GAB.vertex 6;
            GAB.edge 46 v4 v6;
            GAB.edge 54 v5 v4;
            pure ()
        }

        let g3 = GAB.build $ do {
            v7 <- GAB.vertex 7;
            v8 <- GAB.vertex 8;
            v9 <- GAB.vertex 9;
            GAB.edge 97 v9 v7;
            GAB.edge 98 v9 v8;
            pure ()
        }

        let g' = GAB.build $ do {
            GAB.vertex g1;
            GAB.vertex g2;
            GAB.vertex g3;
            pure ()
        }

        let g'' = GAI.flatten g'

        GA.vertices g'' `shouldMatchList` [1, 2, 3, 4, 5, 6, 7, 8, 9]
        GA.edges g'' `shouldMatchList`
            [ (1, 12, 2), (2, 23, 3)
            , (4, 46, 6), (5, 54, 4)
            , (9, 97, 7), (9, 98, 8)
            ]

    it "should convert edges between graphs into edges between all pairs of vertices" $ do

        let g1 = GAB.build $ do {
            GAB.vertex 1;
            GAB.vertex 2;
            pure ()
        }

        let g2 = GAB.build $ do {
            GAB.vertex 3;
            pure ()
        }

        let g3 = GAB.build $ do {
            GAB.vertex 4;
            GAB.vertex 5;
            GAB.vertex 6;
            pure ()
        }

        let g' = GAB.build $ do {
            v1 <- GAB.vertex g1;
            v2 <- GAB.vertex g2;
            v3 <- GAB.vertex g3;

            GAB.edge 13 v1 v3;
            GAB.edge 32 v3 v2;

            pure ()
        }

        let g'' = GAI.flatten g'

        GA.vertices g'' `shouldMatchList` [1, 2, 3, 4, 5, 6]
        GA.edges g'' `shouldMatchList`
            [ (1, 13, 4), (1, 13, 5), (1, 13, 6)
            , (2, 13, 4), (2, 13, 5), (2, 13, 6)
            , (4, 32, 3), (5, 32, 3), (6, 32, 3)
            ]

    it "should flatten a graph of graphs" $ do

        let g1 = GAB.build $ do {
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;
            GAB.edge 21 v2 v1;
            pure ()
        }

        let g2 = GAB.build $ do {
            v3 <- GAB.vertex 3;
            GAB.edge 33 v3 v3;
            pure ()
        }

        let g3 = GAB.build $ do {
            v4 <- GAB.vertex 4;
            v5 <- GAB.vertex 5;
            v6 <- GAB.vertex 6;

            GAB.edge 54 v5 v4;
            GAB.edge 56 v5 v6;
            pure ()
        }

        let g' = GAB.build $ do {
            v1 <- GAB.vertex g1;
            v2 <- GAB.vertex g2;
            v3 <- GAB.vertex g3;

            GAB.edge 1133 v1 v3;
            GAB.edge 3322 v3 v2;

            pure ()
        }

        let g'' = GAI.flatten g'

        GA.vertices g'' `shouldMatchList` [1, 2, 3, 4, 5, 6]
        GA.edges g'' `shouldMatchList`
            [ (1, 1133, 4), (1, 1133, 5), (1, 1133, 6)
            , (2, 1133, 4), (2, 1133, 5), (2, 1133, 6)
            , (4, 3322, 3), (5, 3322, 3), (6, 3322, 3)
            , (2, 21, 1), (3, 33, 3), (5, 54, 4), (5, 56, 6)
            ]

fmapSpec :: Spec
fmapSpec = describe "fmap" $ do

    it "should preserve empty graph" $ do

        let g = fmap (+ 1) GAC.empty

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should not change graph with id function" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";

            GAB.edge "e1" b a;
            GAB.edge "e2" c a;
            GAB.edge "e3" c b
        }
        let g' = fmap id g

        GA.vertices g' `shouldMatchList` ["a", "b", "c"]
        GA.edges g' `shouldMatchList` [("b", "e1", "a"), ("c", "e2", "a"), ("c", "e3", "b")]

    it "should not change graph structure" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;

            GAB.edge "e1" v1 v0;
            GAB.edge "e2" v2 v0;
            GAB.edge "e3" v2 v1
        }
        let g' = fmap (+ 1) g

        GA.vertices g' `shouldMatchList` [1, 2, 3]
        GA.edges g' `shouldMatchList` [(2, "e1", 1), (3, "e2", 1), (3, "e3", 2)]

apSpec :: Spec
apSpec = describe "ap" $ do

    it "should return an empty graph if graph of functions is empty" $ do

        let gx = GAC.singleton ()
        let g' = GAC.empty <*> gx

        length (GA.vertices g') `shouldBe` 0
        length (GA.edges g') `shouldBe` 0

    it "should return an empty graph if graph of values is empty" $ do

        let gf = GAC.singleton id
        let g' = gf <*> GAC.empty

        length (GA.vertices g') `shouldBe` 0
        length (GA.edges g') `shouldBe` 0

    it "should return a graph of all possible combinations" $ do

        let gf = GAB.build $ do {
            f0 <- GAB.vertex (2 +);
            f1 <- GAB.vertex (3 *);

            GAB.edge "f01" f0 f1
        }

        let gx = GAB.build $ do {
            v0 <- GAB.vertex (5 :: Int);
            v1 <- GAB.vertex 30;

            GAB.edge "v10" v1 v0
        }

        let g' = gf <*> gx

        GA.vertices g' `shouldMatchList` [7, 15, 32, 90]
        GA.edges g' `shouldMatchList`
            [ (7, "f01", 15), (32, "f01", 90)
            , (7, "f01", 90), (32, "f01", 15)
            , (90, "v10", 15), (32, "v10", 7)
            ]

bindSpec :: Spec
bindSpec = describe "bind" $ do

    it "should return an empty graph if function returns an empty graph" $ do

        let g = GAC.singleton ()
        let g' = g >>= (const GAC.empty)

        length (GA.vertices g') `shouldBe` 0
        length (GA.edges g') `shouldBe` 0

    it "should return an empty graph if original graph is empty" $ do

        let g = GAC.empty >>= id

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should return a graph with duplicated vertices" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;

            GAB.edge "01" v0 v1;
            GAB.edge "12" v1 v2
        }

        let f n = GAC.isolated (L.replicate n n)

        let g' = g >>= f

        GA.vertices g' `shouldMatchList` [1, 2, 2]
        GA.edges g' `shouldMatchList` [ (1, "12", 2), (1, "12", 2) ]


foldrSpec :: Spec
foldrSpec = describe "foldr" $ do

    it "should return a default value on an empty graph" $ do

        let g = GAC.empty

        foldr (+) 42 g `shouldBe` 42

    it "should return a number of vertices with sum function" $ do

        let g = GAC.isolated [1, 1, 1, 1, 1]

        foldr (+) 0 g `shouldBe` 5

traverseSpec :: Spec
traverseSpec = describe "traverse" $ do

    it "should return a pure graph if traversed with pure function" $ do

        let g = GAC.isolated [1, 2, 3]
        let mg = traverse pure g

        M.isJust mg `shouldBe` True

        let (Just g') = mg

        GA.vertices g' `shouldMatchList` [1, 2, 3]
        length (GA.edges g') `shouldBe` 0

    it "should return nothing if one of the vertices is mapped to nothing" $ do

        let g = GAC.isolated [1, 2, 3]

        let f 2 = Nothing
            f _ = Just ()

        M.isNothing (traverse f g) `shouldBe` True


spec :: Spec
spec = describe "Data.Graph.Abstract.Instances" $ do
    flattenSpec
    fmapSpec
    apSpec
    bindSpec
    foldrSpec
    traverseSpec
